#' Get the responses from the feedback survey.
#'
#' @inheritParams get_course_metadata_field
#'
#' @return A [tibble::tibble].
#'
#' @examples
#'
#' \dontrun{
#' get_feedback_survey("intro")
#' get_feedback_survey("general")
#' }
#'
get_feedback_survey <- function(id = "general") {
  # "general" is the newer, generic feedback survey.
  id <- rlang::arg_match(id, c("general", list_course_ids()))

  get_feedback_survey_google_sheet(id) %>%
    convert_to_long(id) %>%
    drop_missing_responses() %>%
    remove_newlines("response") %>%
    add_course_date(id) |>
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

add_course_date <- function(data, id) {
  id <- rlang::arg_match(id, c("general", list_course_ids()))

  if (id == "general") {
    data <- tibble::tibble(
      course_id = list_course_ids(),
      course_name = purrr::map_chr(course_id, ~ get_course_metadata_field(.x, "name"))
    ) |>
      dplyr::right_join(data, by = "course_name") |>
      dplyr::mutate(course_date = purrr::map2_chr(
        timestamp,
        course_id,
        ~ assign_course_date_by_date(
          .x,
          # In case people submit a few days afterwards.
          lubridate::as_date(get_course_dates(.y)) + lubridate::days(3)
        )
      ))
  } else if (id %in% c("intro", "inter", "adv")) {
    data <- data %>%
      dplyr::mutate(
        course_date = assign_course_date_by_date(
          timestamp,
          # In case people submit a few days afterwards.
          lubridate::as_date(get_course_dates(id)) +
            lubridate::days(3)
        )
      ) |>
      dplyr::mutate(course_id = id)
  }

  data |>
    # Correct the course date from the assigning function.
    dplyr::mutate(course_date = lubridate::as_date(course_date) - lubridate::days(3)) |>
    dplyr::relocate(course_id, course_date)
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
