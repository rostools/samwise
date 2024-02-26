#' Extract participant overview data from pre-course survey.
#'
#' @param data Pre-course survey data.
#'
#' @return A tibble.
#' @export
#'
extract_participant_overview <- function(data) {
  data |>
    anonymize_precourse() |>
    dplyr::select(
      course_version,
      tidyselect::starts_with("perceived"),
      tidyselect::starts_with("uses"),
      gender_identity,
      research_position,
      city_work_in,
      previously_used_stat_programs,
      accept_conduct
    ) |>
    tidyr::pivot_longer(
      -course_version,
      names_to = "questions",
      values_to = "responses"
    ) |>
    dplyr::count(course_version, questions, responses, name = "count") |>
    dplyr::arrange(course_version, questions, responses, count) |>
    join_original_column_names(column_renaming_df) %>%
    dplyr::mutate(
      questions = questions %>%
        stringr::str_replace(
          "^How .* perceive .*\\.\\.\\. \\[(.*)\\]$",
          "Perceived skill/knowledge in \\1"
        ) %>%
        stringr::str_remove_all("\\[|\\]")
    ) |>
    dplyr::relocate(course_version, questions, responses, count)
}

#' @describeIn extract_precourse Extract and tidy up the pre-course feedback
#'  data.
#' @export
extract_precourse_feedback <- function(data, column_renaming_df) {
  data |>
    sanitize_precourse() |>
    dplyr::select(
      course_version,
      tidyselect::contains("feedback"),
      describe_problems,
      tidyselect::contains("course_expectations"),
      tidyselect::contains("why_attend_course")
    ) |>
    tidyr::pivot_longer(
      -course_version,
      names_to = "questions",
      values_to = "responses"
    ) |>
    dplyr::arrange(course_version, questions, responses) |>
    join_original_column_names(column_renaming_df) |>
    dplyr::relocate(course_version, questions, responses)
}

anonymize_precourse <- function(data) {
  data |>
    dplyr::select(
      -tidyselect::contains("email"),
      -tidyselect::contains("name"),
      -tidyselect::contains("github_username"),
      -timestamp
    )
}


  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      ~ stringr::str_remove_all(.x, '\\n|\\"')
    )) |>
    dplyr::select(tidyselect::where(~ !all(is.na(.x))))
}

join_original_column_names <- function(data, column_renaming_df) {
  data |>
    dplyr::left_join(column_renaming_df, by = c("questions" = "new_column_names")) |>
    tidyr::drop_na() |>
    dplyr::select(-questions, questions = original_column_names) |>
    dplyr::relocate(questions)
}
