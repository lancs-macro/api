library(jsonlite)
library(here)

write_json2 <- function(...) {
  write_json(..., pretty = TRUE)
}

write_json2(
  data.frame(
    ID = c("datets", "visualizations")
  ),
  here("public", "index.json")
)

write_json2(
  data.frame(
    Description = c("Intenational", "United Kingdom"),
    ID = c("int", "uk")
  ),
  here("public", "datasets", "index.json")
)
