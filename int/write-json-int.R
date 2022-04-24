
library(here)
source(here("int/00-functions.R"))
source(here("int/01-download.R"))

create_new_version(release)

# desc <- list(
#   data = "Data",
#   rhpi = list(
#     radf = "RADF (exuber)",
#     stat = "Exuberance Statistics (GSADF)",
#     seq  = "Exuberance Statitics Sequence (BSADF)",
#     panel = "asdasd",
#     dummy = "Dummy Variable"
#   ),
#   pti = list(
#     radf = "RADF (exuber)", # tidy
#     stat = "Exuberance Statistics (GSADF)", # augment
#     seq  = "Exuberance Statitics Sequence (BSADF)",
#     panel = "asdasd",
#     dummy = "Dummy Variable"
#   ),
#   crit = list(
#     stat = "asdas",
#     seq = "asdasd",
#     panel = list(
#       rhpi = "asdasd",
#       pti = "adsda"
#     )
#   ),
#   psyivx = list(
#     data = "Data",
#     datestamping = "Datestamping"
#   )
# )
desc <- c(
  "Exuberance Statistics and Critical Values (ADF)",
  "Exuberance Statistics and Critical Values (BADF)",
  "Exuberance Statistics and Critical Values (GSADF)",

  "Real House Prices",
  "RADF Real House Price (exuber)",
  "Real House Prices Exuberance Statistics (BSADF)",
  "Real House Prices Exuberance Statistics (Dummy)",

  "House-Price-to-Income-Ratio",
  "RADF House-Price-to-Income-Ratio (exuber)",
  "House-Price-to-Income Exuberance Statistics (BSADF)",
  "Real House Prices Exuberance Statistics (Dummy)",

  "Monte Carlo Critical Values (exuber)",
  "GSADF Critical Value Statistics",
  "BSADF Critical Value Sequence Statistics",

  "PSYIVX Data",
  "PSYIVX Datestamping"
)

ids <- c(
  "stat-adf", 
  "stat-badf", 
  "stat-gsadf",
  
  "rhpi", 
  "rhpi-stat",
  "rhpi-bsadf",
  "rhpi-cv", 
  "rhpi-dummy",
  
  "pti",  
  "exuber-pti",  
  "stat-pti",  
  "seq-pti",  
  "dummy-pti",
  
  "exuber-cv", 
  "stat-cv", 
  "seq-cv",
  
  "psyivx-data", 
  "psyivx-ds"
)

rel <- gsub(" ", "", tolower(release))

write_json2(
  list(
    release = release,
    results = data.frame(Description = desc, ID = ids),
    meta = list(
      count = length(ids)
    )
  ),
  here("public/datasets/int", rel, "index.json")
)

write_json2(
  rhpi,
  here("public/datasets/int", rel, "rhpi.json")
)
serialize_json2(
  radf_rhpi,
  here("public/datasets/int", rel, "radf-rhpi.json")
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
  pti, 
  here("public/datasets/int", rel, "pti.json")
)
serialize_json2(
  radf_pti,
  here("public/datasets/int", rel, "radf-pti.json")
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




