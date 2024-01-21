#' Title
#'
#' @return A (heavily nested) list.
#' @export
#'
#' @examples
#' library(purrr)
#' read_course_metadata() |>
#'   purrr::map(names) |>
#'   unique()
read_course_metadata <- function() {
  courses_path <- fs::path_package(package = "r3admin", "data", "courses.yaml")
  yaml::read_yaml(courses_path)$course
}

list_course_ids <- function() {
  read_course_metadata() |>
    purrr::map_chr("id") |>
    sort()
}

#' Metadata on the dates the courses have started on.
#'
#' @param course_id ID for the course (found in the `courses.yaml` file).
#'
#' @return Character vector of dates.
#' @export
#'
#' @examples
#' get_course_dates("inter")
#' get_course_dates("intro")
#' get_course_dates("adv")
get_course_dates <- function(id = list_course_ids()) {
  get_course_metadata_field(id, "events") |>
    purrr::map_chr(~ purrr::pluck(.x, "date")) |>
    sort()
}

get_course_metadata_field <- function(id, field) {
  id <- rlang::arg_match(id, list_course_ids())
  read_course_metadata() |>
    purrr::keep(~ .x$id == id) |>
    purrr::pluck(1, field)
}

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
#'
#' get_upcoming_course_dates("intro")
#' get_upcoming_course_dates("inter")
#' get_upcoming_course_dates("adv")
get_upcoming_course_dates <- function(id = list_course_ids()) {
  get_course_dates(id = id) |>
    purrr::keep(~ .x >= lubridate::today())
}

get_upcoming_course <- function() {
  course_metadata <- read_course_metadata()

  # upcoming <-
    # course_metadata |>
    # map_depth(3, "date", .ragged = TRUE) |>
    # map("events") |>
    # map(unlist) |>
    # map(\(x) x[x >= lubridate::today()])
    # |>
    # unlist()
    # |>
    # map(\(x) x == min(x)

  read_course_metadata() |>
    map("id") |>
    keep(upcoming_dates)
}

get_course_repo <- function(id) {
  get_course_metadata_field(id, "repo")
}
