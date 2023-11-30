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

#' @describeIn fetch_feedback Fetch the session feedback survey data for the **introductory** course.
#' @export
fetch_feedback_intro <- function(survey_id = Sys.getenv("INTRO_FEEDBACK_SURVEY_ID")) {
  fetch_feedback_generic(survey_id = survey_id, course_id = "intro")
}

#' @describeIn fetch_feedback Fetch the session feedback survey data for the **intermediate** course.
#' @export
fetch_feedback_inter <- function(survey_id = Sys.getenv("INTERMEDIATE_FEEDBACK_SURVEY_ID")) {
  fetch_feedback_generic(survey_id = survey_id, course_id = "inter")
}

#' @describeIn fetch_feedback Fetch the session feedback survey data for the **advanced** course.
#' @export
fetch_feedback_advanced <- function(survey_id = Sys.getenv("ADVANCED_FEEDBACK_SURVEY_ID")) {
  fetch_feedback_generic(survey_id = survey_id, course_id = "adv")
}

#' @describeIn fetch_feedback Fetch the session feedback survey data for all
#'   courses (from the updated single survey).
#' @export
fetch_feedback_general <- function(survey_id = Sys.getenv("GENERAL_FEEDBACK_SURVEY_ID")) {
  fetch_feedback_generic(survey_id = survey_id, course_id = "general")
}

fetch_feedback_generic <- function(survey_id, course_id) {
  survey_id %>%
    fetch_feedback_sheet() %>%
    convert_to_long() %>%
    drop_missing_responses() %>%
    remove_newlines("response") %>%
    add_course_version(course_id) |>
    dplyr::select(-timestamp)
}

fetch_feedback_sheet <- function(survey_id) {
  googledrive::drive_get(id = survey_id) %>%
    googlesheets4::read_sheet(col_types = "c") %>%
    dplyr::mutate(Timestamp = lubridate::mdy_hms(Timestamp),
                  date = lubridate::as_date(Timestamp))
}

# Tidy up feedback data ---------------------------------------------------

add_course_version <- function(data, course_id) {
  few_days_after_course <- lubridate::as_date(metadata_course_dates(course_id)) +
    lubridate::days(3)
  data %>%
    dplyr::mutate(
      course_version = assign_course_version_by_date(
        timestamp,
        few_days_after_course
      )
    ) %>%
    dplyr::relocate(course_version, tidyselect::everything())
}

convert_to_long <- function(data) {
  data %>%
    dplyr::rename(
      day = "Which of the days is the feedback for?",
      timestamp = "Timestamp"
    ) %>%
    dplyr::mutate(id = ids::random_id(n = dplyr::n())) |>
    # check_duplicate_timestamps() %>%
    tidyr::pivot_longer(
      cols = -c(timestamp, day, date, id),
      names_to = "question",
      values_to = "response"
    ) %>%
    dplyr::mutate(question = stringr::str_remove_all(
      question,
      "\\.\\.\\.[0-9][0-9]?"
    ))
}

drop_missing_responses <- function(data) {
  data %>%
    dplyr::filter(
      !is.na(response),
      !response %in% c("na", "NA", ".", "-", "?", "N/A", "Nil")
    )
}

remove_newlines <- function(data, col) {
  data %>%
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(col),
      ~ stringr::str_remove_all(.x, pattern = "\n")
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
