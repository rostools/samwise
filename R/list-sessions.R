
#' Get the session schedule for the course.
#'
#' @inheritParams get_course_metadata_field
#'
#' @return
#' @export
#'
#' @examples
#' get_schedule_sessions("intro")
get_schedule_sessions <- function(id) {
  id <- rlang::arg_match(id, list_course_ids())
  course_repo <- get_course_repo(id = id)
  base_url <- "https://raw.githubusercontent.com/rostools"
  schedule_path <- "preamble/schedule.csv"
  full_url <- glue::glue("{base_url}/{course_repo}/main/{schedule_path}")
  readr::read_csv(full_url, show_col_types = FALSE) |>
    dplyr::filter(icon %in% c("laptop-code", "person-chalkboard")) |>
    dplyr::arrange(Day, Time) |>
    dplyr::select(day = Day, topic = `Session topic`) |>
    dplyr::mutate(topic = stringr::str_remove(topic, "\\(with short break\\)") |>
                    stringr::str_trim()) |>
    dplyr::distinct(day, topic)
}


