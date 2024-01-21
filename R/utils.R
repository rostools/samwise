#' Within date range, from lubridate
#'
#' @name %within%
#' @rdname within
#' @keywords internal
#' @export
#' @importFrom lubridate %within%
#' @param lhs A date.
#' @param rhs An interval object.
#' @return A logical vector.
NULL

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

#' Assign the version of the course that the survey response comes from.
#'
#' @param date The date of the survey response.
#'
#' @return A numeric value.
#' @keywords internal
#'
#' @examples
#' assign_course_version_by_date(c("2020-06-20", "2019-06-10"), get_course_dates("intro"))
assign_course_version_by_date <- function(date, metadata_dates) {
  metadata_dates <- as.character(metadata_dates)
  dates_between_courses <- lubridate::interval(
    c("2018-01-01", metadata_dates),
    c(metadata_dates, as.character(lubridate::today() + lubridate::weeks(4)))
  )
  course_version <- date %>%
    lubridate::as_date() %>%
    lubridate::ymd() %>%
    purrr::map_int(~ head(which(.x %within% dates_between_courses), 1))
  if (length(course_version) == 0) {
    course_version <- NA_integer_
  }
  course_version
}
