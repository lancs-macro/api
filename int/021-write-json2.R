
library(here)
source(here("int/00-functions.R"))
source(here("int/01-download.R"))

rel <- create_new_version(release)

desc <- c(
  
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
  
  "PSYIVX - Data",
  "PSYIVX - Datestamping"
)


ids <- c(
  "rhpi", "rhpi-radf", "rhpi-stat", "rhpi-seqstat", "rhpi-dummy",
  "pti", "pti-radf", "pti-stat", "pti-seqstat", "pti-dummy",
  "crit-mc",
  "psyivx-data", "psyivx-ds"
)


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


# rhpi --------------------------------------------------------------------


write_json2(
  rhpi,
  here("public/datasets/int", rel, "rhpi.json")
)
serialize_json2(
  radf_rhpi,
  here("public/datasets/int", rel, "rhpi-radf.json")
)
write_json2(
  rhpi_stat,
  here("public", "datasets", "int", rel, "rhpi-stat.json")
)
write_json2(
  rhpi_seqstat,
  here("public", "datasets", "int", rel, "rhpi-seqstat.json")
)
write_json2(
  rhpi_dummy,
  here("public", "datasets", "int", rel, "rhpi-dummy.json")
)


# pti ---------------------------------------------------------------------


write_json2(
  pti, 
  here("public/datasets/int", rel, "pti.json")
)
serialize_json2(
  radf_pti,
  here("public/datasets/int", rel, "pti-radf.json")
)
write_json2(
  pti_stat,
  here("public", "datasets", "int", rel, "pti-stat.json")
)
write_json2(
  pti_seqstat,
  here("public", "datasets", "int", rel, "pti-seqstat.json")
)
write_json2(
  pti_dummy,
  here("public", "datasets", "int", rel, "pti-dummy.json")
)

# crit --------------------------------------------------------------------

write_json2(
  mc_cv,
  here("public", "datasets", "int", rel, "crit-mc.json")
)


# psyivx ------------------------------------------------------------------



write_json2(
  psyivx_data, 
  here("public", "datasets", "int", rel, "psyivx-data.json")
)
write_json2(
  psyivx_ds, 
  here("public", "datasets", "int", rel, "psyivx-ds.json")
)




