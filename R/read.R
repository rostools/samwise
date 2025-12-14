#' Read in specific survey results for all years for a workshop.
#'
#' @param id The workshop ID, see [list_workshop_ids()] for a list of them.
#' @param type The survey type, e.g. from the pre-workshop or post-session
#'   feedback.
#'
#' @returns A [tibble::tibble()].
#' @export
read_surveys <- function(
  id,
  type = c(
    "overview",
    "feedback-overall",
    "feedback-quantitative",
    "feedback-sessions"
  )
) {
  type <- rlang::arg_match(type)

  fs::path_package("samwise", "extdata", "surveys", id) |>
    fs::dir_ls(recurse = TRUE, regexp = type) |>
    fs::path_filter(glob = "*.csv") |>
    purrr::map(read_survey) |>
    purrr::list_rbind()
}

read_survey <- function(path) {
  ids_pattern <- stringr::str_c(
    list_workshop_ids(),
    collapse = "|"
  )
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      id = path |>
        stringr::str_remove("^.*extdata/surveys/") |>
        stringr::str_extract(ids_pattern),
      date = path |>
        stringr::str_remove("^.*extdata/surveys/") |>
        stringr::str_extract("\\d{4}-\\d{2}-\\d{2}"),
      .before = tidyselect::everything()
    )
}
