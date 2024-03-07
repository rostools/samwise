#' Get the responses from the feedback survey.
#'
#' @inheritParams get_course_metadata_field
#'
#' @return A [tibble::tibble].
#'
#' @examples
#'
#' get_feedback_survey("intro")
#' get_feedback_survey("general")
#'
get_feedback_survey <- function(id = "general") {
  # "general" is the newer, generic feedback survey.
  id <- rlang::arg_match(id, c("general", list_course_ids()))

  get_feedback_survey_google_sheet(id) %>%
    convert_to_long(id) %>%
    drop_missing_responses() %>%
    remove_newlines("response") %>%
    add_course_version(id) |>
    dplyr::select(-timestamp)
}

get_feedback_survey_google_sheet <- function(id = "general") {
  id <- rlang::arg_match(id, c("general", list_course_ids()))

  # Get the Google Sheet ID from the environment variable via `Sys.getenv()`
  survey_id <- switch(id,
    intro = "INTRO_FEEDBACK_SURVEY_ID",
    inter = "INTERMEDIATE_FEEDBACK_SURVEY_ID",
    adv = "ADVANCED_FEEDBACK_SURVEY_ID",
    general = "GENERAL_FEEDBACK_SURVEY_ID"
  )
  survey_id <- Sys.getenv(survey_id)

  if (survey_id == "") {
    cli::cli_abort("{.fn Sys.genenv} can't find the Google Sheet ID, do you have an {.val .Renviron} set up with the ID?")
  }

  googledrive::drive_get(id = survey_id) %>%
    googlesheets4::read_sheet(col_types = "c") %>%
    dplyr::mutate(
      Timestamp = lubridate::mdy_hms(Timestamp),
      date = lubridate::as_date(Timestamp)
    )
}

# Helpers -----------------------------------------------------------------

add_course_version <- function(data, id) {
  id <- rlang::arg_match(id, c("general", list_course_ids()))

  if (id == "general") {
    data <- tibble::tibble(
      course_id = list_course_ids(),
      course_name = purrr::map_chr(course_id, ~ get_course_metadata_field(.x, "name"))
    ) |>
      dplyr::right_join(data, by = "course_name") |>
      dplyr::mutate(course_version = purrr::map2_int(
        timestamp,
        course_id,
        ~ assign_course_version_by_date(
          .x,
          # In case people submit a few days afterwards.
          lubridate::as_date(get_course_dates(.y)) + lubridate::days(3)
        )
      ))
  } else if (id %in% c("intro", "inter", "adv")) {
    data <- data %>%
      dplyr::mutate(
        course_version = assign_course_version_by_date(
          timestamp,
          # In case people submit a few days afterwards.
          lubridate::as_date(get_course_dates(id)) +
            lubridate::days(3)
        )
      ) |>
      dplyr::mutate(course_id = id)
  }

  data |>
    dplyr::relocate(course_id, course_version)
}

convert_to_long <- function(data, id) {
  id <- rlang::arg_match(id, c("general", list_course_ids()))

  if (id == "general") {
    data <- data %>%
      dplyr::rename(
        course_name = "Which course is the feedback for?",
        session_name = "Which session is the feedback for?",
        timestamp = "Timestamp"
      ) |>
      tidyr::pivot_longer(
        cols = -c(timestamp, date, course_name, session_name),
        names_to = "question",
        values_to = "response"
      )
  } else if (id %in% c("intro", "inter", "adv")) {
    data <- data %>%
      dplyr::rename(
        day = "Which of the days is the feedback for?",
        timestamp = "Timestamp"
      ) |>
      tidyr::pivot_longer(
        cols = -c(timestamp, day, date),
        names_to = "question",
        values_to = "response"
      )
  }

  data %>%
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
