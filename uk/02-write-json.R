source("uk/01-download.R")

# helpers -----------------------------------------------------------------


library(jsonlite)
library(here)


create_new_version <- function(release) {
  rel <- gsub(" ", "", tolower(release))
  fs::dir_create("public/datasets/uk/", rel)
  all_rel <- fs::path_file(fs::dir_ls("public/datasets/uk", type = "directory"))
  write_json2(
    list(
      releases = list(sort(all_rel, decreasing = TRUE))
    ),
    path = here("public", "datasets", "uk", "index.json")
  )
  rel
}


write_json2 <- function(...) {
  jsonlite::write_json(..., pretty = TRUE)
}

write_serial_json2 <- function(x, path, ...) {
  json <- jsonlite::serializeJSON(x, ..., pretty = TRUE)
  writeLines(json, path, useBytes = TRUE)
}

# international -----------------------------------------------------------


rel <- create_new_version(release)


description <- c(
  "Real House Prices - Data",
  "Real House Prices - radf object (exuber)",
  "Real House Prices - Exuberance Statistics",
  "Real House Prices - Exuberance Sequence Statistics",
  "Real House Prices - Dummy Variable",
  "House-Price-to-Income-Ratio - Data",
  "House-Price-to-Income-Ratio - radf object (exuber)",
  "House-Price-to-Income-Ratio - Exuberance Statistics",
  "House-Price-to-Income-Ratio - Exuberance Sequence Statistics",
  "House-Price-to-Income-Ratio - Dummy Variable",
  "Monte Carlo Critical Values (exuber)",
  "Economic Price Uncertainty",
  "House Price Uncertainty",
  "Housing Observatory Price Indices - Aggregate",
  "Housing Observatory Price Indices - NUTS 1",
  "Housing Observatory Price Indices - NUTS 2",
  "Housing Observatory Price Indices - NUTS 3"
)

ids <- c(
  "rhpi",
  "rhpi-radf",
  "rhpi-stat",
  "rhpi-seqstat",
  "rhpi-dummy",
  "pti",
  "pti-radf",
  "pti-stat",
  "pti-seqstat",
  "pti-dummy",
  "crit-mc",
  "epu",
  "hpu",
  "hopi-aggregate",
  "hopi-nuts1",
  "hopi-nuts2",
  "hopi-nuts3"
)


write_json2(
  list(
    release = release,
    results = data.frame(Description = description, ID = ids),
    meta = list(
      count = length(ids)
    )
  ),
  here("public/datasets/uk", rel, "index.json")
)




# rhpi --------------------------------------------------------------------


write_json2(
  rhpi,
  here("public/datasets/uk", rel, "rhpi.json")
)
write_serial_json2(
  radf_rhpi,
  here("public/datasets/uk", rel, "rhpi-radf.json")
)
write_json2(
  rhpi_stat,
  here("public", "datasets", "uk", rel, "rhpi-stat.json")
)
write_json2(
  rhpi_seqstat,
  here("public", "datasets", "uk", rel, "rhpi-seqstat.json")
)
write_json2(
  rhpi_dummy,
  here("public", "datasets", "uk", rel, "rhpi-dummy.json")
)

# pti ---------------------------------------------------------------------


write_json2(
  pti,
  here("public/datasets/uk", rel, "pti.json")
)
write_serial_json2(
  radf_pti,
  here("public/datasets/uk", rel, "pti-radf.json")
)
write_json2(
  pti_stat,
  here("public", "datasets", "uk", rel, "pti-stat.json")
)
write_json2(
  pti_seqstat,
  here("public", "datasets", "uk", rel, "pti-seqstat.json")
)
write_json2(
  pti_dummy,
  here("public", "datasets", "uk", rel, "pti-dummy.json")
)

# crit --------------------------------------------------------------------

write_serial_json2(
  mc_cv,
  here("public", "datasets", "uk", rel, "crit-mc.json")
)

# policy uncertainty ------------------------------------------------------

write_json2(
  epu_index,
  here("public", "datasets", "uk", rel, "epu.json")
)
write_json2(
  hpu_index,
  here("public", "datasets", "uk", rel, "hpu.json")
)

# hopi  ------------------------------------------------------

write_json2(
  hopi_aggregate,
  here("public", "datasets", "uk", rel,"hopi-aggregate.json")
)
write_json2(
  hopi_nuts1,
  here("public", "datasets", "uk", rel, "hopi-nuts1.json")
)
write_json2(
  hopi_nuts2,
  here("public", "datasets", "uk", rel, "hopi-nuts2.json")
)
write_json2(
  hopi_nuts3,
  here("public", "datasets", "uk", rel, "hopi-nuts3.json")
)
