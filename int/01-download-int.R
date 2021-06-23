
suppressMessages({
  library(tidyverse)
  library(readxl)
  library(ihpdr)
  library(here)
  library(exuber)
  library(ivx)
  library(transx)
  options(transx.display = FALSE)
})


# download ----------------------------------------------------------------

full_data <- ihpdr::ihpd_get()

cnames <- ihpdr::ihpd_countries()

rhpi <- select(full_data, Date, country, rhpi) %>% 
  pivot_wider(names_from = country, values_from = rhpi)

pti <- full_data %>% 
  group_by(country) %>% 
  mutate(price_income = rhpi/rpdi) %>% 
  select(Date, country, price_income) %>% 
  pivot_wider(names_from = country, values_from = price_income)


# Analysis ----------------------------------------------------------------

suppressMessages({
  radf_rhpi <- radf(rhpi, lag = 1)
  radf_pti <- radf(pti, lag = 1)
})
mc_cv <- radf_crit[[NROW(rhpi)]]

# psyivx ------------------------------------------------------------------

datafile <- here::here("data-raw", "int", "psyivx.xlsx")

nms0 <- gsub("\\.\\.", "\\.", names(readxl::read_excel(datafile)))[-1]
sheets <- excel_sheets(datafile)
suppressMessages({
  lsheets <- map(
    sheets, ~
      readxl::read_excel(datafile, sheet = .x, skip = 5, col_names = FALSE) %>%
      set_names(c("Date", nms0)) %>%
      mutate(Date = zoo::as.Date(zoo::as.yearqtr(Date, format = "Q%q/%Y")))
  )
})

nms <-
  c("Australia", "Belgium", "Canada", "Denmark", "Finland", "France",
    "Germany", "Ireland", "Netherlands", "New.Zealand", "Norway", "Spain",
    "UK", "US")

vars <- c("hpi", "rent", "ltrate", "credit", "ngdp", "pdi", "un", "resi", "rgdp", "cpi", "pop", "permits")
hdata_raw <- map2(lsheets, vars, ~ pivot_longer(.x, -Date, values_to = .y)) %>%
  reduce(full_join, by = c("Date", "name"))

hdata <- hdata_raw %>%
  filter(name %in% nms) %>% 
  group_by(name) %>%
  mutate(
    rhpi = hpi/cpi*100,
    pti = hpi/pdi*100,
    ptr = hpi/rent*100,
    rrent = rent/cpi*100,
    rpdi = pdi/cpi*100,
    ltrate = ltrate/100,
    un = un/100
  ) %>%
  mutate(
    grhpi = transx::ldiffx(rhpi),
    gpti = transx::ldiffx(pti),
    gptr = transx::ldiffx(ptr),
  ) %>%
  mutate(
    log_hpi = log(hpi),
    log_rent = log(rent),
    log_pdi = log(pdi),
    log_credit = log(credit),
    log_ngdp = log(ngdp),
    log_resi = log(resi),
    log_cpi = log(cpi),
    log_pop = log(pop),
    log_permits = log(permits)
  ) %>%
  mutate(
    log_rhpi  = log(rhpi),
    log_rrent = log(rrent),
    log_rpdi = log(rpdi),
    log_pti = log(pti),
    log_ptr = log(ptr)
  )

tbl_data <- group_split(drop_na(hdata))
names(tbl_data) <- nms

# * helpers ----

ds_fun <- function(x) datestamp(x, min_duration = 2, nonrejected = T)[[1]]
safe_ds_fun <- safely(ds_fun)

# * price-to-rent ----
suppressMessages({
  radf_ptr <- map(tbl_data, ~ select(.x, Date, log_ptr) %>% radf(lag = 1))
  ds_ptr <-  map(radf_ptr, safe_ds_fun) %>% 
    map("result") %>% 
    bind_rows(.id = "name") %>% 
    select(Coucntry = name, Start, Peak, End, Duration)
})



suppressMessages({})


# * estimation ----

residuals_ivx <- function(
  formula, 
  data, 
  predictor = "log_ptr", 
  train_split = "2019-01-01"
) {
  
  train_data <- filter(data, Date <= train_split)
  train_mod <- ivx(formula, data = train_data)
  train_coefs <- c(constant = train_mod$initial$intercept, train_mod$coefficients)
  
  mod <- ivx(formula, data)
  X <- mod$data$X
  y <- data[[predictor]][-1]
  fitted <- cbind(constant = 1, X) %*% train_coefs  
  fitted2 <- y[1] + cumsum(fitted)
  res <- y - fitted2
  
  tibble(Date = data$Date[-1], price = y, bubble = res, fundamentals = fitted2)
}

ivx_data <- map(tbl_data, ~ residuals_ivx(gptr ~ ltrate + log_rent, .x))

suppressMessages({
  radf_ivx <- ivx_data %>% 
    map(~ radf(.x[,c("Date", "bubble")], lag = 1))
  
  ptr_ds <- map(tbl_data, ~ radf(.x[,c("Date", "log_ptr")], lag = 1)) %>% 
    map(safe_ds_fun) %>% 
    map("result") %>% 
    bind_rows(.id = "name") %>% 
    select(name, Start, Peak, End, Duration, Signal)
  
  ivx_ds <-  map(radf_ivx, safe_ds_fun) %>% 
    map("result") %>% 
    bind_rows(.id = "name") %>% 
    select(name, Start, Peak, End, Duration, Signal) %>% 
    select(name, Start, Peak, End, Duration, Signal)
})


# data export -------------------------------------------------------------

release <- as.character(zoo::as.yearqtr(tail(rhpi,1)$Date))

# * stats ----

stat_table <- function(stat = "gsadf") {
  stat_cv <- paste0(stat, "_cv")
  tibble(
    Countries = names(rhpi)[-1],
    `Real House Prices` = radf_rhpi[[stat]],
    `House-Price-Income` = radf_pti[[stat]],
    `90% Critical Values` = mc_cv[[stat_cv]][1],
    `95% Critical Values` = mc_cv[[stat_cv]][2],
    `99% Critical Values` = mc_cv[[stat_cv]][3]
  )
}

adf_table <- stat_table("adf")
sadf_table <- stat_table("sadf")
gsadf_table <- stat_table("gsadf")

# * series ----

estimation_rhpi <- augment(radf_rhpi) %>% 
  select(-data, -badf, -key) %>% 
  pivot_wider(names_from = "id", values_from = "bsadf")

estimation_pti <- augment(radf_pti) %>% 
  select(-data, -badf, -key) %>% 
  pivot_wider(names_from = "id", values_from = "bsadf")

cv_seq <- mc_cv %>% 
  .$bsadf_cv %>% 
  as_tibble() %>% 
  "["(-1,) %>% 
  bind_cols(Date = index(radf_rhpi, trunc = TRUE)) %>% 
  select(Date, everything())

# * dummies ----

estimation_rhpi_dummy <- datestamp(radf_rhpi, mc_cv) %>%
  attr("dummy") %>% 
  as_tibble() %>% 
  add_column(Date = index(radf_rhpi)) %>% 
  select(Date, everything())

estimation_pti_dummy <- datestamp(radf_pti, mc_cv) %>%
  attr("dummy") %>% 
  as_tibble() %>% 
  add_column(Date = index(radf_pti)) %>% 
  select(Date, everything())

# * psyivx datestamping ----

psyivx_ds <- bind_rows(list(ptr = ptr_ds, psyivx = ivx_ds), .id = "type") %>% 
  

# * psyivx data ----

psyivx_data <- full_join(
  bind_rows(ivx_data, .id = "name"),
  select(hdata, Date, log_rent, ltrate),
  by = c("Date", "name")
) 

