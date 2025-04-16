library(here)



path <-  here("data-raw", "int", "hp2404.xlsx")

ihpd_get_raw <- function(tf) {
  
  list(
    ihpdr:::format_excel_raw(tf, 2, "hpi"),
    ihpdr:::format_excel_raw(tf, 3, "rhpi"),
    ihpdr:::format_excel_raw(tf, 4, "pdi"),
    ihpdr:::format_excel_raw(tf, 5, "rpdi")) %>%
    reduce(full_join, by = c("Date", "country")) %>%
    drop_na() 
}


ihpd_get_local <- function(path) {
  ihpd_get_raw(path)
}



