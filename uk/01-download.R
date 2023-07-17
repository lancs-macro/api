library(tidyverse)
library(readxl)
library(here)

source(here("uk/00-functions.R"))

# Naming  -----------------------------------------------------------------

nms <- tibble::tribble(
  ~num, ~abbr, ~ntwd, ~names,
  1L, "EA", "East Anglia", "East Anglia",
  2L, "EM", "East Mids", "East Midlands",
  3L, "GL", "London", "Greater London",
  4L, "NT", "North", "North",
  5L, "NW", "North West", "North West",
  4L, "NI", "N Ireland", "Northern Ireland",
  7L, "OM", "Outer Met", "Outer Metropolitan",
  8L, "OSE", "Outer S East", "Outer South East",
  9L, "SC", "Scotland", "Scotland",
  10L, "SW", "South West", "South West",
  11L, "UK", "UK", "United Kingdom",
  12L, "WW", "Wales", "Wales",
  13L, "WM", "West Mids", "West Midlands",
  14L, "YH", "Yorks & Hside", "Yorkshire & Humberside"
)

abbr_to_names <- pull(nms, names) %>%
  set_names(pull(nms, abbr))

ntwd_to_names <- pull(nms, names) %>%
  set_names(pull(nms, ntwd))

# uklr to hopi regions ----------------------------------------------------

hp_nms <- tibble::tribble(
  ~num, ~names, ~hopi, ~lr,
  1L, "East of England", "East of England", "east-of-england",
  2L, "West Midlands", "West Midlands (England)", "west-midlands",
  3L, "South West", "South West (England)", "south-west",
  4L, "North West", "North West (England)", "north-west",
  5L, "Yorkshire and The Humber", "Yorkshire and The Humber", "yorkshire-and-the-humber",
  6L, "South East", "South East (England)", "south-east",
  7L, "London", "London", "london",
  8L, "North East", "North East (England)", "north-east",
  9L, "Wales", "Wales", "wales",
  10L, "East Midlands", "East Midlands (England)", "east-midlands"
)

# Download CPI index ------------------------------------------------------

download.file(
  "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.CPI.TOT.IDX2015.Q/OECD?contentType=csv&detail=code&separator=comma&csv-lang=en&startPeriod=1973-Q1",
  "data-raw/uk/cpi.csv"
)

# Reading ntwd ------------------------------------------------------------

hpi <-
  nationwider::ntwd_get("seasonal_regional", verbose = FALSE) %>%
  dplyr::filter(type == "Index", Date >= "1975-01-01") %>%
  select(-type, hpi = value) %>%
  mutate(region = recode(region, "Uk" = "United Kingdom")) %>%
  mutate(region = recode(region, !!!ntwd_to_names))



last_obs <- select(hpi, Date, region)


# read_csv(
#   "data-raw/uk/cpi.csv",
#   col_types = cols_only(
#     LOCATION = col_guess(),
#     TIME = col_guess(),
#     Value = col_guess()
#   )
# ) 

cpi <- read.csv("data-raw/uk/cpi.csv" )%>%
  tibble::tibble() %>% 
  dplyr::filter(LOCATION == "GBR") %>%
  dplyr::select(TIME, Value) %>%
  dplyr::rename(Date = TIME, cpi = Value) %>%
  mutate(Date = Date %>%
    zoo::as.yearqtr(format = "%Y-Q%q") %>%
    zoo::as.Date())

rpdi <- read_excel("data-raw/uk/rpdi.xlsx") %>%
  mutate(Date = Date %>%
    zoo::as.yearqtr(format = "Q%q %Y") %>%
    zoo::as.Date()) %>%
  gather(region, rpdi, -Date) %>%
  mutate(region = recode(region, !!!abbr_to_names)) %>%
  right_join(last_obs, by = c("Date", "region")) %>%
  group_by(region) %>%
  mutate(rpdi = imputeTS::na_interpolation(rpdi, option = "spline")) %>%
  ungroup()


ntwd_data <- right_join(hpi, rpdi, by = c("region", "Date")) %>%
  right_join(cpi, by = "Date") %>%
  drop_na() %>%
  mutate(rhpi = hpi / cpi, pti = rhpi / rpdi)


rhpi <- ntwd_data %>%
  select(Date, region, rhpi) %>%
  spread(region, rhpi)

pti <- ntwd_data %>%
  select(Date, region, pti) %>%
  spread(region, pti)



release <- as.character(zoo::as.yearqtr(tail(rhpi,1)$Date))

# estmation ---------------------------------------------------------------

library(exuber)

radf_rhpi <- rhpi %>%
  radf(lag = 1, minw = 37)

radf_pti <- pti %>%
  radf(lag = 1, minw = 37)

mc_cv <- radf_mc_cv(NROW(rhpi), minw = 37)


# * dummies ----

radf_rhpi_dummy <- datestamp(radf_rhpi, mc_cv) %>%
  attr("dummy") %>%
  as_tibble() %>%
  add_column(Date = index(radf_rhpi)) %>%
  select(Date, everything())

radf_pti_dummy <- datestamp(radf_pti, mc_cv) %>%
  attr("dummy") %>%
  as_tibble() %>%
  add_column(Date = index(radf_pti)) %>%
  select(Date, everything())



# hopi --------------------------------------------------------------------

# TODO fetch from own calculations

latest <- fs::dir_ls("data-raw/uk/data_hopi", type = "dir") %>% 
  fs::path_file() %>% 
  tail(1)

hopi_aggregate <- readr::read_csv(here("data-raw/uk/data_hopi", latest, "quarterly", "aggregate.csv")) %>% 
  select(Date, `England and Wales` = `United Kingdom`) %>% 
  mutate(Date = lubridate::yq(Date))

hopi_nuts1 <- readr::read_csv(here("data-raw/uk/data_hopi", latest, "quarterly", "nuts1.csv")) %>% 
  mutate(Date = lubridate::yq(Date))

hopi_nuts2 <- readr::read_csv(here("data-raw/uk/data_hopi", latest, "quarterly", "nuts2.csv")) %>% 
  mutate(Date = lubridate::yq(Date))

hopi_nuts3 <- readr::read_csv(here("data-raw/uk/data_hopi", latest, "quarterly", "nuts3.csv")) %>% 
  mutate(Date = lubridate::yq(Date))


# Download EPU Index ------------------------------------------------------

download.file("https://www.policyuncertainty.com/media/UK_Policy_Uncertainty_Data.xlsx",
  "data-raw/uk/epu.xlsx",
  mode = "wb"
)

# EPU ---------------------------------------------------------------------

epu_index <-
  readxl::read_xlsx("data-raw/uk/epu.xlsx") %>%
  slice(-n()) %>%
  mutate(year_month = paste(year, month)) %>%
  mutate(year_month = lubridate::ymd(year_month, truncated = 1)) %>%
  select(Date = year_month, EPU = UK_EPU_Index) %>%
  group_by(Date = zoo::as.Date(zoo::as.yearqtr(Date, "%b-%Y"))) %>%
  summarise_all(mean) %>%
  drop_na()

# HPU ---------------------------------------------------------------------

hpu_index <-
  readxl::read_excel("data-raw/uk/hpu.xlsx") %>%
  mutate(year_quarter = paste(Year, Quarter)) %>%
  mutate(year_quarter = lubridate::yq(year_quarter)) %>%
  select(Date = year_quarter, HPU) %>%
  full_join(epu_index, by = "Date")


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
  "["(-1, ) %>%
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

