#' Get the responses from the feedback survey.
#'
#' @return A [tibble::tibble].
#'
#' @examples
#' \dontrun{
#' get_feedback_survey()
#' }
#'
get_feedback_survey <- function() {
  feedback <- get_feedback_survey_google_sheet() |>
    relocate_session_column() |>
    drop_empty_columns() |>
    convert_to_long() |>
    drop_missing_responses() |>
    remove_newlines("response") |>
    add_course_date() |>
    dplyr::select(-timestamp)

  if (!"session_name" %in% names(feedback)) {
    feedback <- feedback |>
      dplyr::mutate(session_name = "End of course")
  }

  feedback
}

get_feedback_survey_google_sheet <- function() {
  # Get the Google Sheet ID from the environment variable via `Sys.getenv()`
  survey_id <- Sys.getenv("GENERAL_FEEDBACK_SURVEY_ID")

  if (survey_id == "") {
    cli::cli_abort(
      "{.fn Sys.genenv} can't find the Google Sheet ID, do you have an {.val .Renviron} set up with the ID?"
    )
  }

  googledrive::drive_get(id = survey_id) |>
    googlesheets4::read_sheet(col_types = "c") |>
    dplyr::mutate(
      Timestamp = lubridate::mdy_hms(Timestamp),
      date = lubridate::as_date(Timestamp)
    )
}

# Helpers -----------------------------------------------------------------

drop_empty_columns <- function(data) {
  data |>
    dplyr::select(
      tidyselect::where(\(x) !all(is.na(x)))
    )
}

relocate_session_column <- function(data) {
  data |>
    dplyr::relocate(
      tidyselect::contains("session is feedback for"),
      .after = tidyselect::contains("Which workshop is")
    )
}

add_course_date <- function(data) {
  tibble::tibble(
    course_id = list_course_ids(),
    course_name = purrr::map_chr(
      course_id,
      ~ get_course_metadata_field(.x, "name")
    )
  ) |>
    dplyr::right_join(data, by = "course_name") |>
    dplyr::mutate(
      course_date = purrr::map2_chr(
        timestamp,
        course_id,
        ~ assign_course_date_by_date(
          .x,
          # In case people submit a few days afterwards.
          lubridate::as_date(get_course_dates(.y)) + lubridate::days(3)
        )
      )
    ) |>
    # Correct the course date from the assigning function.
    dplyr::mutate(
      course_date = lubridate::as_date(course_date) - lubridate::days(3)
    ) |>
    dplyr::relocate(course_id, course_date)
}

convert_to_long <- function(data) {
  data |>
    dplyr::rename(
      course_name = "Which workshop is the feedback for?",
      timestamp = "Timestamp"
    ) |>
    tidyr::pivot_longer(
      cols = -c(
        timestamp,
        date,
        course_name,
        tidyselect::contains("session is the feedback for")
      ),
      names_to = "question",
      values_to = "response"
    ) |>
    dplyr::rename_with(
      \(col)
        dplyr::if_else(
          stringr::str_detect(col, "session is the feedback for"),
          "session_name",
          col
        ),
    ) |>
    dplyr::mutate(
      question = stringr::str_remove_all(
        question,
        "\\.\\.\\.[0-9][0-9]?"
      )
    )
}

drop_missing_responses <- function(data) {
  data |>
    dplyr::filter(
      !is.na(response),
      !stringr::str_trim(response) %in% c("na", "NA", ".", "-", "?", "N/A", "Nil", "%", "no comment", "no comments")
    )
}

remove_newlines <- function(data, col) {
  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(col),
      ~ stringr::str_remove_all(.x, pattern = "\n")
    ))
}
