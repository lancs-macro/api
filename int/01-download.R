
suppressMessages({
  library(tidyverse)
  library(readxl)
  library(ihpdr) #github
  library(here)
  library(exuber)
  library(ivx) # github
  library(transx) # github
  options(transx.display = FALSE)
})


# Download ----------------------------------------------------------------

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


# Release -----------------------------------------------------------------

release <- as.character(zoo::as.yearqtr(tail(rhpi,1)$Date))

rel <- tolower(gsub(" ", "", release))

# psyivx q1 ---------------------------------------------------------------

datafile <- here::here("data-raw", "int", glue::glue("psyivx-{rel}.xlsx"))
nms0 <- gsub("\\.\\.", "\\.", names(readxl::read_excel(datafile)))[-1]
sheets <- excel_sheets(datafile)
suppressMessages({
  lsheets <- map(
    sheets, ~
      readxl::read_excel(datafile, sheet = .x, col_names = TRUE) %>% 
      rename("Date" = ...1) %>% 
      mutate(Date = zoo::as.Date(zoo::as.yearqtr(Date, format = "Q%q/%Y")))
  )
})

nms <- c("Australia", "Belgium", "Canada", "Denmark", "Finland", "France",
    "Germany", "Ireland", "New Zealand", "Norway", "Spain", "US")

vars <- c("hpi", "rent", "ltrate")
hdata_raw <- map2(lsheets, vars, ~ pivot_longer(.x, -Date, values_to = .y, names_to = "country")) %>%
  reduce(full_join, by = c("Date", "country")) %>% 
  mutate(country = recode(country,  "New.Zealand" = "New Zealand"))

hdata <- hdata_raw %>%
  filter(country %in% nms) %>% 
  group_by(country) %>%
  mutate(
    ptr = hpi/rent*100,
    ltrate = ltrate/100
  ) %>%
  mutate(
    gptr = transx::ldiffx(ptr),
  ) %>%
  mutate(
    log_hpi = log(hpi),
    log_rent = log(rent),
    log_ptr = log(ptr)
  ) 

tbl_data <- group_split(drop_na(hdata))
names(tbl_data) <- nms

# * helpers ----

ds_fun <- function(x) datestamp(x, min_duration = 2, nonrejected = T)
safe_ds_fun <- safely(ds_fun)

# * estimation ----

data = tbl_data[[1]]
formula = gptr ~ ltrate + log_rent
predictor = "log_ptr"
train_split = "2019-01-01"


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
  
  tibble(Date = data$Date[-1], price = y, bubble = res, fundamental = fitted2)
}

ivx_data <- map(tbl_data, ~ residuals_ivx(gptr ~ ltrate + log_rent, .x))

suppressMessages({
  
  radf_bubble <- ivx_data %>% 
    map(~ radf(.x[,c("Date", "bubble")], lag = 1))
  nn <- map_dbl(ivx_data, nrow)
  
  bsadf_bubble <- radf_bubble %>% 
    map(~ augment_join(.x) %>% filter(stat == "bsadf", sig == 95) %>% select(index, tstat, crit)) %>% 
    bind_rows(.id = "country") %>% 
    set_names(c("country", "Date", "bsadf_bubble", "bsadf_cv"))
  
  ds_bubble <-  map(radf_bubble, safe_ds_fun) %>% 
    map("result")
  # TODO this breaks here
  is_zero_length <- function(x) length(x) != 0
  is_nonrejected <- map_lgl(ds_bubble, is_zero_length)
  
  extra_dummy_bubble <- list()
  for(i in names(ds_bubble)[is_nonrejected]) {
    ni <- nn[i]
    extra_dummy_bubble[[i]] <- tibble(index(radf_bubble[[i]]), rep(0, ni)) %>% 
      set_names(c("Date", i))
  }
  extra_dummy_bubble <- reduce(extra_dummy_bubble, full_join, by = "Date")
  
  ds_bubble <- compact(ds_bubble)
  dummy_bubble <- map(ds_bubble, ~ attr(.x, "dummy")) %>% 
    map2(ds_bubble, ~ as_tibble(.x) %>% mutate(Date = index(.y))) %>% 
    reduce(full_join, by = "Date") %>% 
    relocate(Date) %>% 
    set_names(c("Date", names(ds_bubble))) %>%
    full_join(extra_dummy_bubble, by = "Date") %>% 
    pivot_longer(-Date, names_to = "country" , values_to = "dummy_bubble")
  
  datestamp_bubble <- ds_bubble %>%
    map(~ .x[[1]]) %>% 
    bind_rows(.id = "country") %>% 
    select(country, Start, Peak, End, Duration, Signal) 
  
  radf_ptr <- map(tbl_data, ~ radf(.x[,c("Date", "log_ptr")], lag = 1))
  
  bsadf_ptr <- radf_ptr %>% 
    map(~ augment_join(.x) %>% filter(stat == "bsadf", sig == 95) %>% select(index, tstat)) %>% 
    bind_rows(.id = "country") %>% 
    set_names(c("country", "Date", "bsadf_price"))
  
  ds_ptr <- map(radf_ptr, safe_ds_fun) %>% 
    map("result")
  
  dummy_ptr <-  compact(ds_ptr)
  dummy_ptr <- map(dummy_ptr, ~ attr(.x, "dummy")) %>% 
    map2(dummy_ptr, ~ as_tibble(.x) %>% mutate(Date = index(.y))) %>% 
    reduce(full_join, by = "Date") %>% 
    relocate(Date) %>% 
    set_names(c("Date", names(dummy_ptr))) %>%
    pivot_longer(-Date, names_to = "country" , values_to = "dummy_ptr")
  
  datestamp_ptr <- ds_ptr %>%
    map(~ .x[[1]]) %>% 
    bind_rows(.id = "country") %>% 
    select(country, Start, Peak, End, Duration, Signal) 

})

# EXPORT ------------------------------------------------------------------



# * stats ----


# rhpi --------------------------------------------------------------------

rhpi_stat <- tidy_join(radf_rhpi, mc_cv)
rhpi_seqstat <- augment_join(radf_rhpi, mc_cv)
rhpi_dummy <- datestamp(radf_rhpi, mc_cv) %>%
  attr("dummy") %>% 
  as_tibble() %>% 
  add_column(index = index(radf_rhpi)) %>% 
  select(index, everything()) %>% 
  pivot_longer(-index, names_to = c("id"), values_to = "dummy")



# pti ---------------------------------------------------------------------

pti_stat <- tidy_join(radf_pti, mc_cv)
pti_seqstat <- augment_join(radf_pti, mc_cv)
pti_dummy <- datestamp(radf_pti, mc_cv) %>%
  attr("dummy") %>% 
  as_tibble() %>% 
  add_column(index = index(radf_pti)) %>% 
  select(index, everything()) %>% 
  pivot_longer(-index, names_to = c("id"), values_to = "dummy")


# -------------------- OLD DATA ---------------------------------

stat_table <- function(stat = "gsadf") {
  stat_cv <- paste0(stat, "_cv")
  tibble(
    Countries = names(rhpi)[-1],
    `Real House Prices` = radf_rhpi[[stat]],
    # `House-Price-Income` = radf_pti[[stat]],
    `90% Critical Values` = mc_cv[[stat_cv]][1],
    `95% Critical Values` = mc_cv[[stat_cv]][2],
    `99% Critical Values` = mc_cv[[stat_cv]][3]
  )
}

adf_table <- stat_table("adf")
sadf_table <- stat_table("sadf")
gsadf_table <- stat_table("gsadf")

# augment(radf_rhpi, mc_cv)


# * series ----

rhpi_bsadf <- augment(radf_rhpi) %>% 
  select(-data, -badf, -key) %>% 
  pivot_wider(names_from = "id", values_from = "bsadf")

pti_bsadf <- augment(radf_pti) %>% 
  select(-data, -badf, -key) %>% 
  pivot_wider(names_from = "id", values_from = "bsadf")

cv_bsadf <- mc_cv %>% 
  .$bsadf_cv %>% 
  as_tibble() %>% 
  "["(-1,) %>% 
  bind_cols(Date = index(radf_rhpi, trunc = TRUE)) %>% 
  select(Date, everything())

# * dummies ----

rhpi_dummy <- datestamp(radf_rhpi, mc_cv) %>%
  attr("dummy") %>% 
  as_tibble() %>% 
  add_column(Date = index(radf_rhpi)) %>% 
  select(Date, everything())

pti_dummy <- datestamp(radf_pti, mc_cv) %>%
  attr("dummy") %>% 
  as_tibble() %>% 
  add_column(Date = index(radf_pti)) %>% 
  select(Date, everything())


# PSYIVX ------------------------------------------------------------------


# * psyivx datestamping ----

psyivx_ds <- bind_rows(list(ptr = datestamp_ptr, psyivx = datestamp_bubble), .id = "type")

# * psyivx data ----

psyivx_data <- full_join(
  bind_rows(ivx_data, .id = "country"),
  select(hdata, Date, log_rent, ltrate),
  by = c("Date", "country")
) %>% 
  full_join(
    list(bsadf_ptr, bsadf_bubble, dummy_ptr, dummy_bubble) %>% 
      reduce(full_join, by = c("country", "Date")),
    by = c("Date", "country")
  )

