
library(tidyverse)
library(readxl)

# Naming  -----------------------------------------------------------------

nms <- tibble::tribble(
  ~num,   ~abbr,             ~ntwd,                   ~names,
  1,   "EA",      "East Anglia",               "East Anglia",
  2,   "EM",        "East Mids",              "East Midlands",
  3,   "GL",           "London",           "Greater London",
  4,   "NT",            "North",                    "North",
  5,   "NW",       "North West",               "North West",
  4,   "NI",        "N Ireland",         "Northern Ireland",
  7,   "OM",        "Outer Met",       "Outer Metropolitan",
  8,  "OSE",     "Outer S East",         "Outer South East",
  9,   "SC",         "Scotland",                 "Scotland",
  10,   "SW",       "South West",               "South West",
  11,   "UK",               "UK",           "United Kingdom",
  12,   "WW",            "Wales",                    "Wales",
  13,   "WM",        "West Mids",            "West Midlands",
  14,   "YH",    "Yorks & Hside",   "Yorkshire & Humberside",
)

abbr_to_names <- c(
  "EA" = "East Anglia", 
  "EM" = "East Midlands", 
  "GL" = "Greater London",
  "NI" = "Northern Ireland", 
  "NT" = "North",
  "NW" = "North West",
  "OM" = "Outer Metropolitan",
  "OSE" = "Outer South East",
  "SC" = "Scotland",
  "SW" = "South West",
  "UK" = "United Kingdom",
  "WM" = "West Midlands",
  "WW" =  "Wales",                 
  "YH" =  "Yorkshire & Humberside"
)

ntwd_to_names <- c(
  "East Anglia" = "East Anglia", 
  "East Mids" = "East Midlands", 
  "London" = "Greater London",
  "N Ireland" = "Northern Ireland", 
  "North" = "North",
  "North West" = "North West",
  "Outer Met" = "Outer Metropolitan",
  "Outer S East" = "Outer South East",
  "Scotland" = "Scotland",
  "South West" = "South West",
  "UK" = "United Kingdom",
  "West Mids" = "West Midlands",
  "Wales" =  "Wales",                 
  "Yorks & Hside" =  "Yorkshire & Humberside"
)

# Download CPI index ------------------------------------------------------

download.file("https://stats.oecd.org/sdmx-json/data/DP_LIVE/.CPI.TOT.IDX2015.Q/OECD?contentType=csv&detail=code&separator=comma&csv-lang=en&startPeriod=1973-Q1",
              "data-raw/cpi.csv")


# Reading ntwd ------------------------------------------------------------

hpi <- 
  nationwider::ntwd_get("seasonal_regional", verbose = FALSE) %>% 
  dplyr::filter(type == "Index", Date >= "1975-01-01") %>% 
  select(-type, hpi = value) %>% 
  mutate(region = recode(region, "Uk" = "United Kingdom")) %>% 
  mutate(region = recode(region, !!!ntwd_to_names))

last_obs <- select(hpi, Date, region)

cpi <- 
  readr::read_csv(
    "data-raw/cpi.csv", 
    col_types = 
      cols_only(LOCATION = col_guess(), 
                TIME = col_guess(), 
                Value = col_guess())) %>% 
  dplyr::filter(LOCATION == "GBR") %>% 
  dplyr::select(TIME, Value) %>% 
  dplyr::rename(Date = TIME, cpi = Value) %>% 
  mutate(Date = Date %>% 
           zoo::as.yearqtr(format = "%Y-Q%q") %>%
           zoo::as.Date()
  ) 

rpdi <- read_excel("data-raw/rpdi.xlsx") %>%
  mutate(Date = Date %>% 
           zoo::as.yearqtr(format = "Q%q %Y") %>%
           zoo::as.Date()
  ) %>% 
  gather(region, rpdi, -Date) %>% 
  mutate(region = recode(region, !!!abbr_to_names)) %>% 
  right_join(last_obs, by = c("Date", "region")) %>% 
  group_by(region) %>% 
  mutate(rpdi = imputeTS::na_interpolation(rpdi, option = "spline")) %>% 
  ungroup() 


ntwd_data <- right_join(hpi, rpdi, by = c("region" ,"Date")) %>%
  right_join(cpi, by = "Date") %>% 
  drop_na() %>% 
  mutate(rhpi = hpi/cpi, afford = rhpi/rpdi) 


price <- ntwd_data %>% 
  select(Date, region, rhpi) %>% 
  spread(region, rhpi)

afford <- ntwd_data %>% 
  select(Date, region, afford) %>% 
  spread(region, afford)

release_date <- price %>%
  tail(1) %>% 
  mutate(version = paste0(lubridate::year(Date), " Q",lubridate::quarter(Date))) %>% 
  pull(version)

# hopi --------------------------------------------------------------------


ukhp_get(frequency = "quarterly", classification = "aggregate") %>% 
  select(Date, `England and Wales`) %>% 
  saveRDS("data/aggregate_data.rds")

ukhp_get(frequency = "quarterly", classification = "nuts1") %>% 
  saveRDS("data/nuts1_data.rds")

ukhp_get(frequency = "quarterly", classification = "nuts2") %>% 
  saveRDS("data/nuts2_data.rds")

ukhp_get(frequency = "quarterly", classification = "nuts3") %>% 
  saveRDS("data/nuts3_data.rds")

# Download EPU Index ------------------------------------------------------

download.file("https://www.policyuncertainty.com/media/UK_Policy_Uncertainty_Data.xlsx",
              "data-raw/epu.xlsx", mode = 'wb')

# HPU ---------------------------------------------------------------------

hpu_index <-
  readxl::read_excel("data-raw/hpu.xlsx") %>% 
  mutate(year_quarter = paste(Year, Quarter)) %>% 
  mutate(year_quarter = lubridate::yq(year_quarter)) %>% 
  select(Date = year_quarter, HPU)

# EPU ---------------------------------------------------------------------

epu_index <-
  readxl::read_xlsx("data-raw/epu.xlsx") %>%
  slice(-n()) %>% 
  mutate(year_month = paste(year, month)) %>%
  mutate(year_month = lubridate::ymd(year_month, truncated = 1)) %>%
  select(Date = year_month, EPU = UK_EPU_Index) %>%
  group_by(Date = zoo::as.Date(zoo::as.yearqtr(Date, "%b-%Y"))) %>%
  summarise_all(mean) %>% 
  drop_na()


