


library(jsonlite)

write_json2 <- function(...) {
  jsonlite::write_json(..., pretty = TRUE)
}

# international -----------------------------------------------------------

desc <- c(
  "Real House Prices",
  "House-Price-to-Income-Ratio",
  "Exuberance Statistics and Critical Values (GSADF)",
  "Real House Prices Exuberance Statistics (BSADF)",
  "Real House Prices Exuberance Statistics (Dummy)",
  "House-Price-to-Income Exuberance Statistics (BSADF)",
  "Real House Prices Exuberance Statistics (Dummy)",
  "BSADF Critical Value Sequence Statistics",
  "PSYIVX Data",
  "PSYIVX Datestamping"
)
ids <- c("rhpi", "pti", "seq-rhpi", "dummy-rhpi", "seq-pti", "dummy-pti", "seq-cv", "stat", "psyivx_data", "psyivx_ds")

write_json2(
  list(
    release = release,
    results = data.frame(Description = desc, ID = ids),
    meta = list(
      count = length(ids)
    )
  ),
  here("public", "datasets", "int", "index.json")
)

write_json2(
  rhpi,
  here("public", "datasets", "int", "rhpi.json")
)
write_json2(
  pti, 
  here("public", "datasets", "int", "pti.json")
)
write_json2(
  estimation_rhpi,
  here("public", "datasets", "int", "seq-rhpi.json")
)
write_json2(
  estimation_rhpi_dummy,
  here("public", "datasets", "int", "dummy-rhpi.json")
)
write_json2(
  estimation_pti,
  here("public", "datasets", "int", "seq-pti.json")
)
write_json2(
  estimation_pti_dummy,
  here("public", "datasets", "int", "dummy-pti.json")
)
write_json2(
  cv_seq,
  here("public", "datasets", "int", "seq-cv.json")
)
write_json2(
  gsadf_table, 
  here("public", "datasets", "int", "stat.json")
)
write_json2(
  psyivx_data, 
  here("public", "datasets", "int", "psyivx_data.json")
)
write_json2(
  psyivx_ds, 
  here("public", "datasets", "int", "psyivx_ds.json")
)



