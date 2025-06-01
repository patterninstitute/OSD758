dataset_path <- function(dataset) {

  dataset_file <- paste0(dataset, ".rds")
  system.file(file.path("extdata", dataset_file), package = pkg_name(), mustWork = TRUE)
}
