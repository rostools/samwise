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

#' Assign the date of the course that the survey response comes from.
#'
#' @param date The date of the survey response.
#'
#' @return A character value.
#' @keywords internal
#'
#' @examples
#' assign_course_date_by_date(
#'   c("2020-06-20", "2019-06-10", "2021-06-10"),
#'   get_course_dates("intro")
#' )
assign_course_date_by_date <- function(date, metadata_dates) {
  metadata_dates <- as.character(metadata_dates)
  dates_between_courses <- lubridate::interval(
    c("2018-01-01", metadata_dates),
    c(metadata_dates, as.character(lubridate::today() + lubridate::weeks(4)))
  )
  date <- date %>%
    lubridate::as_date() %>%
    lubridate::ymd()

  course_date <- date |>
    purrr::map_chr(
      \(date)
        if (date %in% metadata_dates) {
          stringr::str_subset(metadata_dates, as.character(date))
        } else {
          metadata_dates[which(date %within% dates_between_courses)]
        }
    )
  if (length(course_date) == 0) {
    course_date <- NA_integer_
  }
  course_date
}
