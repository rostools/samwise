#' Get the session schedule for the course.
#'
#' @inheritParams get_course_metadata_field
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @examples
#' get_schedule_sessions("intro")
#' get_schedule_sessions("inter")
#' get_schedule_sessions("adv")
get_schedule_sessions <- function(id) {
  id <- rlang::arg_match(id, list_workshop_ids())
  base_url <- get_course_metadata_field(id = id, field = "url")
  schedule_path <- "overview/schedule"
  full_url <- glue::glue("{base_url}/{schedule_path}")
  scraped_page <- rvest::read_html(full_url)

  days <- scraped_page |>
    rvest::html_elements(".panel-tabset") |>
    rvest::html_elements(".nav-link") |>
    rvest::html_text()

  ignore <- "[sS]urvey|Lunch|Arrival|[nN]etwork|Continue|Coffee|Closing|TBD|[pP]roject|Hands-on|Group|Break"
  scraped_page |>
    rvest::html_table() |>
    purrr::set_names(days) |>
    purrr::list_rbind(names_to = "Days") |>
    dplyr::filter(stringr::str_detect(
      `Session topic`,
      ignore,
      negate = TRUE
    )) |>
    dplyr::mutate(
      `Session topic` = `Session topic` |>
        stringr::str_remove("\\(with short break\\)|Short review;") |>
        stringr::str_trim()
    ) |>
    dplyr::select(-Time) |>
    dplyr::distinct()
}
