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
    "Germany", "Ireland", "New Zealand", "Norway", "Spain", "US")

vars <- c("hpi", "rent", "ltrate", "credit", "ngdp", "pdi", "un", "resi", "rgdp", "cpi", "pop", "permits")
hdata_raw <- map2(lsheets, vars, ~ pivot_longer(.x, -Date, values_to = .y, names_to = "country")) %>%
  reduce(full_join, by = c("Date", "country")) %>% 
  mutate(country = recode(country,  "New.Zealand" = "New Zealand"))

hdata <- hdata_raw %>%
  filter(country %in% nms) %>% 
  group_by(country) %>%
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