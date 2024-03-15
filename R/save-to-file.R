#' Save data to CSV.
#'
#' @param data Survey data.
#' @param path Path where data should be saved to.
#'
#' @return Save a CSV file, returns tibble invisibly.
#' @export
#'
save_as_csv <- function(data, path) {
  fs::dir_create(fs::path_dir(path))
  data |>
    readr::write_csv(here::here(path))
  path
}
