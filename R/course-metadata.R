#' Read in the course's metadata (as a YAML) saved in the package's `data/`
#' folder.
#'
#' @return A (heavily nested) list.
#' @export
#'
#' @examples
#' read_course_metadata()
read_course_metadata <- function() {
  courses_path <- fs::path_package(package = "r3admin", "data", "courses.yaml")
  yaml::read_yaml(courses_path)$course
}

#' List all the IDs for the courses.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' list_course_ids()
list_course_ids <- function() {
  read_course_metadata() |>
    purrr::map_chr("id") |>
    sort()
}

#' List all the metadata fields available in the first-level for each course.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' list_course_metadata_fields()
list_course_metadata_fields <- function() {
  read_course_metadata() |>
    purrr::pluck(1) |>
    names() |>
    stringr::str_subset("id", negate = TRUE)
}

#' Metadata on the dates the courses have started on.
#'
#' @inheritParams get_course_metadata_field
#'
#' @return Character vector of dates.
#' @export
#'
#' @examples
#' get_course_dates("inter")
#' get_course_dates("intro")
#' get_course_dates("adv")
get_course_dates <- function(id) {
  get_course_metadata_field(id, "events") |>
    purrr::map_chr(~ purrr::pluck(.x, "date")) |>
    sort()
}

#' General purpose function
#'
#' @param id The ID of the course, found by running [list_course_ids()].
#' @param field The "key" value (field) for the ID of the course, found by running [list_course_metadata_fields()].
#'
#' @return A (nested) list.
#' @export
#'
#' @examples
#'
#' get_course_metadata_field("intro", "events")
#' get_course_metadata_field("intro", "date")
get_course_metadata_field <- function(id, field) {
  id <- rlang::arg_match(id, list_course_ids())
  field <- rlang::arg_match(field, list_course_metadata_fields())
  read_course_metadata() |>
    purrr::keep(~ .x$id == id) |>
    purrr::pluck(1, field)
}

#' Get the dates for the next course.
#'
#' @inheritParams get_course_metadata_field
#'
#' @return A character vector.
#' @export
#'
#' @examples
#'
#' get_upcoming_course_dates("intro")
#' get_upcoming_course_dates("inter")
#' get_upcoming_course_dates("adv")
get_upcoming_course_dates <- function(id) {
  get_course_dates(id = id) |>
    purrr::keep(~ .x >= lubridate::today())
}

get_upcoming_course <- function() {
  course_metadata <- read_course_metadata()

  upcoming <-
    course_metadata |>
    purrr::map("events") |>
    purrr::map_depth(2, "date", .ragged = TRUE) |>
    purrr::map(unlist) |>
    purrr::map_lgl(\(x) {
      x <- x[x >= lubridate::today()]
      if (!length(x)) {
        x <- NA
      }
      !is.na(min(x))
    })

  course_id <- NA
  if (all(upcoming)) {
    course_id <- read_course_metadata() |>
      purrr::map("id") |>
      purrr::keep(upcoming) |>
      unlist()
  }

  course_id
}

get_course_repo <- function(id) {
  get_course_metadata_field(id, "repo")
}
