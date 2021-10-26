
library(jsonlite)

write_json2 <- function(...) {
  jsonlite::write_json(..., pretty = TRUE)
}

# international -----------------------------------------------------------

create_new_version <- function(release) {
  rel <- gsub(" ", "", tolower(release))
  fs::dir_create("public/datasets/int/", rel)
  all_rel <- fs::path_file(fs::dir_ls("public/datasets/int", type = "directory")) 
  write_json2(
    list(
      releases = list(sort(all_rel))
    ), 
    path = here("public", "datasets", "int", "index.json")
  )
}
create_new_version(release)

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

rel <- gsub(" ", "", tolower(release))

write_json2(
  list(
    release = release,
    results = data.frame(Description = desc, ID = ids),
    meta = list(
      count = length(ids)
    )
  ),
  here("public", "datasets", "int", rel, "index.json")
)

write_json2(
  rhpi,
  here("public", "datasets", "int", rel, "rhpi.json")
)
write_json2(
  pti, 
  here("public", "datasets", "int", rel, "pti.json")
)
write_json2(
  estimation_rhpi,
  here("public", "datasets", "int", rel, "seq-rhpi.json")
)
write_json2(
  estimation_rhpi_dummy,
  here("public", "datasets", "int", rel, "dummy-rhpi.json")
)
write_json2(
  estimation_pti,
  here("public", "datasets", "int", rel, "seq-pti.json")
)
write_json2(
  estimation_pti_dummy,
  here("public", "datasets", "int", rel, "dummy-pti.json")
)
write_json2(
  cv_seq,
  here("public", "datasets", "int", rel, "seq-cv.json")
)
write_json2(
  gsadf_table, 
  here("public", "datasets", "int", rel, "stat.json")
)
write_json2(
  psyivx_data, 
  here("public", "datasets", "int", rel, "psyivx_data.json")
)
write_json2(
  psyivx_ds, 
  here("public", "datasets", "int", rel, "psyivx_ds.json")
)




