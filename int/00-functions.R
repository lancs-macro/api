
library(jsonlite)

write_json2 <- function(...) {
  jsonlite::write_json(..., pretty = TRUE)
}

write_serial_json2 <- function(x, path, ...) {
  json <- jsonlite::serializeJSON(x, ..., pretty = TRUE)
  writeLines(json, path, useBytes = TRUE)
}

# international -----------------------------------------------------------

create_new_version <- function(release) {
  rel <- gsub(" ", "", tolower(release))
  fs::dir_create("public/datasets/int/", rel)
  all_rel <- fs::path_file(fs::dir_ls("public/datasets/int", type = "directory")) 
  write_json2(
    list(
      releases = list(sort(all_rel, decreasing = TRUE))
    ), 
    path = here("public", "datasets", "int", "index.json")
  )
  rel
}