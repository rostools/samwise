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
    add_workshop_date() |>
    dplyr::select(-timestamp)

  if (!"session_name" %in% names(feedback)) {
    feedback <- feedback |>
      dplyr::mutate(session_name = "End of workshop")
  }

  feedback
}

get_feedback_survey_google_sheet <- function() {
  # Get the Google Sheet ID from the environment variable via `Sys.getenv()`
  survey_id <- Sys.getenv("FEEDBACK_SURVEY_ID")

  if (survey_id == "") {
    cli::cli_abort(
      "{.fn Sys.getenv} can't find the Google Sheet ID, do you have an {.val .Renviron} set up with the ID?"
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

add_workshop_date <- function(data) {
  tibble::tibble(
    workshop_id = stringr::str_subset(
      list_workshop_ids(),
      "general",
      negate = TRUE
    ),
    workshop_name = purrr::map_chr(
      workshop_id,
      ~ get_workshop_metadata_field(.x, "name")
    )
  ) |>
    dplyr::right_join(data, by = "workshop_name") |>
    dplyr::mutate(
      workshop_date = purrr::map2_chr(
        timestamp,
        workshop_id,
        ~ assign_workshop_date_by_date(
          .x,
          # In case people submit a few days afterwards.
          lubridate::as_date(get_workshop_dates(.y)) + lubridate::days(10)
        )
      )
    ) |>
    # Correct the workshop date from the assigning function.
    dplyr::mutate(
      workshop_date = lubridate::as_date(workshop_date) - lubridate::days(10)
    ) |>
    dplyr::relocate(workshop_id, workshop_date)
}

convert_to_long <- function(data) {
  data |>
    dplyr::rename(
      workshop_name = "Which workshop is the feedback for?",
      timestamp = "Timestamp"
    ) |>
    tidyr::pivot_longer(
      cols = -c(
        timestamp,
        date,
        workshop_name,
        tidyselect::contains("session is the feedback for")
      ),
      names_to = "question",
      values_to = "response"
    ) |>
    dplyr::rename_with(
      \(col) {
        dplyr::if_else(
          stringr::str_detect(col, "session is the feedback for"),
          "session_name",
          col
        )
      },
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
      !stringr::str_trim(response) %in%
        c(
          "na",
          "NA",
          ".",
          "-",
          "?",
          "N/A",
          "Nil",
          "%",
          "no comment",
          "no comments"
        )
    )
}

remove_newlines <- function(data, col) {
  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(col),
      ~ stringr::str_remove_all(.x, pattern = "\n")
    ))
}
