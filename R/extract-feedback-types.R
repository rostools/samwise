#' Extract the quantitative responses from the feedback survey.
#'
#' @name extract_feedback
#' @rdname extract_feedback
#' @param data The feedback data.
#'
#' @return A tibble.
#'
NULL

#' @describeIn extract_feedback Extract and tidy up the quantitative responses
#'   from the feedback survey data.
#' @export
extract_feedback_quantitative <- function(data) {
  data %>%
    dplyr::filter(stringr::str_detect(question, "Please complete these .*")) %>%
    dplyr::mutate(
      statement = stringr::str_remove(question, "Please .* course. ") %>%
        stringr::str_remove_all("\\[|\\]")
    ) %>%
    dplyr::select(tidyselect::all_of(c(
      "course_version", "date", "statement", "response"
    ))) |>
    dplyr::count(course_version, date, statement, response) |>
    dplyr::arrange(course_version, date)
}

#' @describeIn extract_feedback Extract and tidy up the overall comments
#'   from the feedback survey data.
#' @export
extract_feedback_overall <- function(data) {
  data %>%
    dplyr::filter(stringr::str_detect(
      question,
      ".*any other (feedback|comments) .*"
    )) %>%
    dplyr::rename(overall_comments = response) %>%
    dplyr::select(-question, -id, -day) %>%
    dplyr::filter(stringr::str_detect(
      overall_comments,
      "^No$",
      negate = TRUE
    )) %>%
    dplyr::arrange(course_version, date)
}

#' @describeIn extract_feedback Extract and tidy up the session specific
#'   comments from the feedback survey data.
#' @export
extract_feedback_sessions <- function(data) {
  data %>%
    dplyr::filter(stringr::str_detect(question,
      "Please complete these .*|any other (feedback|comments) .*",
      negate = TRUE
    )) %>%
    dplyr::mutate(question = question %>%
      stringr::str_remove_all('What|session|\\"|\\?') %>%
      stringr::str_trim()) %>%
    tidyr::separate(
      col = question,
      into = c("worked_or_improve", "session"),
      sep = "for the"
    ) %>%
    dplyr::mutate(
      worked_or_improve = snakecase::to_snake_case(worked_or_improve),
      session = stringr::str_trim(session)
    ) %>%
    tidyr::pivot_wider(
      names_from = worked_or_improve,
      values_from = response
    ) %>%
    dplyr::select(-id) %>%
    dplyr::arrange(course_version, date, day, session)
}
