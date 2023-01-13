# Import feedback survey data ---------------------------------------------

#' Fetch the session and overall course feedback survey responses.
#'
#' @name fetch_feedback
#' @rdname fetch_feedback
#' @param survey_id Google Forms ID for the survey.
#'
#' @return Data from survey, slightly tidied up.
#'
#' @examples
#' fetch_feedback_intro()
#' fetch_feedback_advanced()
#'
NULL

#' @describeIn fetch_feedback Fetch the session feedback survey data for the **advanced** course.
#' @export
fetch_feedback_advanced <- function(survey_id = Sys.getenv("ADVANCED_FEEDBACK_SURVEY_ID")) {
  fetch_feedback_generic(survey_id = survey_id)
}

fetch_feedback_generic <- function(survey_id) {
  survey_id %>%
    fetch_feedback_sheet() %>%
    convert_to_long() %>%
    drop_missing_responses() %>%
    remove_newlines("response") %>%
    add_course_version()
}

fetch_feedback_sheet <- function(survey_id) {
  googledrive::drive_get(id = survey_id) %>%
    googlesheets4::read_sheet()
}

# Tidy up feedback data ---------------------------------------------------

add_course_version <- function(data) {
  few_days_after_course <- lubridate::as_date(metadata$dates$advanced) +
    lubridate::days(3)
  data %>%
    dplyr::mutate(
      course_version = assign_course_version_by_date(
        .data$timestamp,
        few_days_after_course
      )
    ) %>%
    dplyr::relocate(.data$course_version, tidyselect::everything())
}

convert_to_long <- function(data) {
  data %>%
    dplyr::rename(
      day = .data$`Which of the days is the feedback for?`,
      timestamp = .data$Timestamp
    ) %>%
    check_duplicate_timestamps() %>%
    tidyr::pivot_longer(
      cols = -c(.data$timestamp, .data$day),
      names_to = "question",
      values_to = "response"
    ) %>%
    dplyr::mutate(question = stringr::str_remove_all(
      .data$question,
      "\\.\\.\\.[0-9][0-9]?"
    ))
}

drop_missing_responses <- function(data) {
  data %>%
    dplyr::filter(
      !is.na(.data$response),
      !.data$response %in% c("na", "NA", ".", "-")
    )
}

remove_newlines <- function(data, col) {
  data %>%
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(col),
      stringr::str_remove_all,
      pattern = "\n"
    ))
}

# Extract types of feedback -----------------------------------------------

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
    dplyr::group_by(.data$timestamp) %>%
    dplyr::mutate(
      statement = stringr::str_remove(.data$question, "Please .* course. ") %>%
        stringr::str_remove_all("\\[|\\]"),
      id = ids::random_id(1, 5)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(c(
      "course_version", "id", "statement", "response"
    )))
}

#' @describeIn extract_feedback Extract and tidy up the overall comments
#'   from the feedback survey data.
#' @export
extract_feedback_overall <- function(data) {
  data %>%
    dplyr::filter(stringr::str_detect(
      .data$question,
      ".*any other comments .*"
    )) %>%
    dplyr::rename(overall_comments = .data$response) %>%
    dplyr::select(-.data$timestamp, -.data$question) %>%
    dplyr::filter(stringr::str_detect(
      .data$overall_comments,
      "^No$",
      negate = TRUE
    ))
}

#' @describeIn extract_feedback Extract and tidy up the session specific
#'   comments from the feedback survey data.
#' @export
extract_feedback_sessions <- function(data) {
  data %>%
    dplyr::filter(stringr::str_detect(.data$question,
      "Please complete these .*|any other comments .*",
      negate = TRUE
    )) %>%
    dplyr::mutate(question = .data$question %>%
      stringr::str_remove_all('What|session|\\"|\\?') %>%
      stringr::str_trim()) %>%
    tidyr::separate(
      col = .data$question,
      into = c("worked_or_improve", "session"),
      sep = "for the"
    ) %>%
    dplyr::mutate(
      worked_or_improve = snakecase::to_snake_case(.data$worked_or_improve),
      session = stringr::str_trim(.data$session)
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$worked_or_improve,
      values_from = .data$response
    ) %>%
    dplyr::select(-.data$timestamp) %>%
    dplyr::arrange(.data$day, .data$session)
}
