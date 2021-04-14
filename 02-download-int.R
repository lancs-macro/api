
library(tidyverse)
library(ihpdr)
library(here)

# download ----------------------------------------------------------------

full_data <- ihpdr::ihpd_get()

cnames <- ihpdr::ihpd_countries()

price <- select(full_data, Date, country, rhpi) %>% 
  pivot_wider(names_from = country, values_from = rhpi)

income <- select(full_data, Date, country, rpdi) %>% 
  pivot_wider(names_from = country, values_from = rpdi)

price_income <- full_data %>% 
  group_by(country) %>% 
  mutate(price_income = rhpi/rpdi) %>% 
  select(Date, country, price_income) %>% 
  pivot_wider(names_from = country, values_from = price_income)


# analysis ----------------------------------------------------------------

library(exuber)

# Estimation and critical values

radf_price <- 
  price %>% 
  radf(lag = 1)

radf_income <- 
  price_income %>% 
  radf(lag = 1)

mc_con <- radf_crit[[NROW(price)]]

# data export -------------------------------------------------------------

estimation_price <- 
  radf_price %>%
  .$bsadf %>% 
  as_tibble() %>% 
  mutate(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())

estimation_income <- 
  radf_income %>%
  .$bsadf %>% 
  as_tibble() %>% 
  mutate(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())

cv_seq <- mc_con %>% 
  .$bsadf_cv %>% 
  as_tibble() %>% 
  "["(-1,) %>% 
  bind_cols(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())

stat_table <- function(stat = "gsadf") {
  stat_cv <- paste0(stat, "_cv")
  tibble(
    Countries = names(price)[-1],
    `Real House Prices` = radf_price[[stat]],
    `House-Price-Income` = radf_income[[stat]],
    `90% Critical Values` = mc_con[[stat_cv]][1],
    `95% Critical Values` = mc_con[[stat_cv]][2],
    `99% Critical Values` = mc_con[[stat_cv]][3]
  )
}

adf_table <- stat_table("adf")
sadf_table <- stat_table("sadf")
gsadf_table <- stat_table("gsadf")

# desc <- c(
#   "Real House Prices",
#   "House-Price-to-Income-Ratio",
#   "Exuberance Statistics and Critical Values (GSADF)",
#   "Real House Prices Exuberance Statistics (BSADF)",
#   "House-Price-to-Income Exuberance Statistics (BSADF)",
#   "BSADF Critical Value Sequence Statistics"
# )
# ids <- c("rhpi", "pti", "seq-rhpi", "seq-pti", "seq-cv", "stat")
# write_json(data.frame(Description = desc, ID = ids), here("public", "datasets", "int", "index.json"))


if(yesno::yesno2("Would you like update?")) {
  library(jsonlite)
  write_json(
    price,
    here("public", "datasets", "int", "rhpi.json")
  )
  write_json(
    price_income, 
    here("public", "datasets", "int", "pti.json")
  )
  write_json(
    estimation_price,
    here("public", "datasets", "int", "seq-rhpi.json")
  )
  write_json(
    estimation_income,
    here("public", "datasets", "int", "seq-pti.json")
  )
  write_json(
    cv_seq,
    here("public", "datasets", "int", "seq-cv.json")
  )
  write_json(
    gsadf_table, 
    here("public", "datasets", "int", "stat.json")
  )
}
