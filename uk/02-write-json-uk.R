
source("uk/01-download-uk.R")

# helpers -----------------------------------------------------------------


library(jsonlite)
library(here)

write_json2 <- function(...) {
  write_json(..., pretty = TRUE)
}

# write_json2(
#   data.frame(
#     Description = c("Intenational", "United Kingdom"),
#     ID = c("int", "uk")
#   ),
#   here("public", "datasets", "index.json")
# )


# international -----------------------------------------------------------

desc <- c(
  "Real House Prices",
  "House-Price-to-Income-Ratio",
  "Exuberance Statistics and Critical Values (GSADF)",
  "Real House Prices Exuberance Statistics (BSADF)",
  "House-Price-to-Income Exuberance Statistics (BSADF)",
  "BSADF Critical Value Sequence Statistics",
  "House Price Uncertainty",
  "Housing Observatory Price Indices - Aggregate",
  "Housing Observatory Price Indices - NUTS 1",
  "Housing Observatory Price Indices - NUTS 2",
  "Housing Observatory Price Indices - NUTS 3"
)
ids <- c("rhpi", "pti", "seq-rhpi", "seq-pti", "seq-cv", "stat", "hpu",
         "hopi-aggregate", "hopi-nuts1", "hopi-nuts2", "hopi-nuts3")

write_json2(
  list(
    results = data.frame(Description = desc, ID = ids), 
    meta = list(
      count = length(ids)
    )
  ),
  here("public", "datasets", "uk", "index.json")
)


# contents ----------------------------------------------------------------


write_json2(
  rhpi,
  here("public", "datasets", "uk", "rhpi.json")
)
write_json2(
  pti, 
  here("public", "datasets", "uk", "pti.json")
)
write_json2(
  estimation_rhpi,
  here("public", "datasets", "uk", "seq-rhpi.json")
)
write_json2(
  estimation_pti,
  here("public", "datasets", "uk", "seq-pti.json")
)
write_json2(
  cv_seq,
  here("public", "datasets", "uk", "seq-cv.json")
)
write_json2(
  gsadf_table, 
  here("public", "datasets", "uk", "stat.json")
)
write_json2(
  hpu_index, 
  here("public", "datasets", "uk", "hpu.json")
)
write_json2(
  hopi_aggregate, 
  here("public", "datasets", "uk", "hopi-aggregate.json")
)
write_json2(
  hopi_nuts1, 
  here("public", "datasets", "uk", "hopi_nuts1.json")
)
write_json2(
  hopi_nuts2, 
  here("public", "datasets", "uk", "hopi_nuts2.json")
)
write_json2(
  hopi_nuts3, 
  here("public", "datasets", "uk", "hopi_nuts3.json")
)


