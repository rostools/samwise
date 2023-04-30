#' Metadata on the dates the courses have started on.
#'
#' @param course_id ID for the course (found in the `courses.yaml` file).
#'
#' @return Character vector of dates.
#' @export
#'
#' @examples
#' metadata_course_dates("inter")
#' metadata_course_dates("intro")
metadata_course_dates <- function(course_id = c("intro", "inter", "adv")) {
  course_id <- rlang::arg_match(course_id)
  course_data_path <- fs::path_package(package = "r3admin", "data", "courses.yaml")
  yaml::read_yaml(course_data_path)$course |>
    purrr::keep(\(x) grepl(pattern = course_id, x$id)) |>
    purrr::pluck(1, "events") |>
    purrr::map_chr(\(x) purrr::pluck(x, "date"))
}


