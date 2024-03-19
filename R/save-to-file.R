#' Save data to CSV.
#'
#' @param data Survey data.
#' @param path Path where data should be saved to.
#'
#' @return Save a CSV file, returns tibble invisibly.
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' general_feedback <- get_feedback_survey("general")
#' extract_feedback_quantitative(general_feedback) |> save_feedback_to_csv(c("course_id", "course_version", "date"))
#' group_split(course_id, course_version, date) |>
#' walk(~ save_to_csv(.x, create_path_from_columns(.x, c("course_id", "course_version", "date"))))
#' View()
#'
save_to_csv <- function(data, path) {
  fs::dir_create(fs::path_dir(path))
  data |>
    readr::write_csv(here::here(path))
  path
}

create_path_from_columns <- function(data, columns) {
  data |>
    dplyr::select({{ columns }}) |>
    dplyr::distinct() |>
    dplyr::mutate(dplyr::across({{ columns }}, as.character)) |>
    purrr::pmap(fs::path) |>
    purrr::map(~usethis::proj_path("data", .x))
}

save_feedback_to_csv <- function(data, columns) {
  data |>
    tidyr::nest(.by = {{ columns }}) |>
    dplyr::mutate(dplyr::across({{ columns }}, as.character)) |>
    dplyr::mutate(path = fs::path(dplyr::c_across({{columns}})))
# purrr::map_chr(~ {
#   path <- create_path_from_columns(.x, c("course_id", "course_version", "date")))
#   save_to_csv(.x,
#   })

}
