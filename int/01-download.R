library(tidyverse)
data <- ihpdr::ihpd_get("raw") %>% 
  select(Date, country, rhpi) %>% 
  pivot_wider(values_from = rhpi, names_from = country)



jsonlite::write_json(mtcars, here::here("public", "index.json"))

