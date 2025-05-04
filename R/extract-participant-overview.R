#' Extract participant overview data from pre-course survey.
#'
#' @param data Pre-course survey data.
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- get_precourse_survey("adv")
#' extract_participant_overview(survey)
#' }
extract_participant_overview <- function(data) {
  if (nrow(data) == 0) {
    cli::cli_warn("No data found in the pre-workshop survey.")
    return(NULL)
  }
  data |>
    anonymize_precourse() |>
    dplyr::select(
      course_id,
      course_date,
      tidyselect::contains("perceive"),
      tidyselect::contains("currently_use"),
      tidyselect::contains("gender"),
      tidyselect::contains("position"),
      tidyselect::contains("city"),
      tidyselect::contains("programs"),
      tidyselect::contains("code_of_conduct")
    ) |>
    tidyr::pivot_longer(
      -c(course_id, course_date),
      names_to = "questions",
      values_to = "responses"
    ) |>
    remove_newlines("responses") |>
    dplyr::count(
      course_id,
      course_date,
      questions,
      responses,
      name = "count"
    ) |>
    dplyr::arrange(course_date, questions, responses, count) |>
    join_original_column_names(id = unique(data$course_id)) |>
    tidyr::drop_na() |>
    dplyr::relocate(course_id, course_date, questions, responses, count)
}

# Helpers -----------------------------------------------------------------

anonymize_precourse <- function(data) {
  data |>
    dplyr::select(
      -tidyselect::contains("email"),
      -tidyselect::contains("name"),
      -tidyselect::contains("github_username"),
      -timestamp
    )
}

get_precourse_survey_column_names <- function(id) {
  column_names <- get_precourse_survey_google_sheet(id, n_max = 0)
  names(column_names)
}

join_original_column_names <- function(data, id) {
  tibble::tibble(
    original = get_precourse_survey_column_names(id),
    converted = snakecase::to_snake_case(original)
  ) |>
    dplyr::right_join(
      data,
      by = c("converted" = "questions")
    ) |>
    dplyr::select(-converted, questions = original)
}
