#' Extract and tidy up the pre-course feedback data.
#'
#' @inheritParams extract_participant_overview
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- get_precourse_survey("intro")
#' extract_precourse_feedback(survey)
#' }
extract_precourse_feedback <- function(data) {
  data |>
    anonymize_precourse() |>
    dplyr::select(
      course_id,
      course_date,
      tidyselect::matches("worked.*well"),
      tidyselect::matches("could.*improved"),
      tidyselect::matches("describe.*problems"),
      tidyselect::matches("expect.*learn"),
      tidyselect::matches("why.*attend")
    ) |>
    tidyr::pivot_longer(
      -c(course_id, course_date),
      names_to = "questions",
      values_to = "responses"
    ) |>
    remove_newlines("responses") |>
    dplyr::arrange(course_date, questions, responses) |>
    join_original_column_names(id = unique(data$course_id)) |>
    tidyr::drop_na() |>
    dplyr::relocate(course_id, course_date, questions, responses)
}
