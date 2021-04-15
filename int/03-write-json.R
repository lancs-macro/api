


library(jsonlite)

write_json2 <- function(...) {
  write_json(..., pretty = TRUE)
}



# international -----------------------------------------------------------

desc <- c(
  "Real House Prices",
  "House-Price-to-Income-Ratio",
  "Exuberance Statistics and Critical Values (GSADF)",
  "Real House Prices Exuberance Statistics (BSADF)",
  "House-Price-to-Income Exuberance Statistics (BSADF)",
  "BSADF Critical Value Sequence Statistics"
)
ids <- c("rhpi", "pti", "seq-rhpi", "seq-pti", "seq-cv", "stat")

write_json2(
  list(
    results = data.frame(Description = desc, ID = ids), 
    meta = list(
      count = length(ids)
    )
  ),
  here("public", "datasets", "int", "index.json")
)

write_json2(
  price,
  here("public", "datasets", "int", "rhpi.json")
)
write_json2(
  price_income, 
  here("public", "datasets", "int", "pti.json")
)
write_json2(
  estimation_price,
  here("public", "datasets", "int", "seq-rhpi.json")
)
write_json2(
  estimation_income,
  here("public", "datasets", "int", "seq-pti.json")
)
write_json2(
  cv_seq,
  here("public", "datasets", "int", "seq-cv.json")
)
write_json2(
  gsadf_table, 
  here("public", "datasets", "int", "stat.json")
)



