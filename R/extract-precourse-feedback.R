
#' Extract and tidy up the pre-course feedback data.
#'
#' @inheritParams extract_participant_overview
#'
#' @return A [tibble::tibble()].
#' @export
#'
#' @examples
#'
#' survey <- get_precourse_survey("intro")
#' extract_precourse_feedback(survey, "intro")
#'
extract_precourse_feedback <- function(data, id) {
  id <- rlang::arg_match(id, list_course_ids())
  data |>
    anonymize_precourse() |>
    dplyr::select(
      course_version,
      tidyselect::matches("worked.*well"),
      tidyselect::matches("could.*improved"),
      tidyselect::matches("describe.*problems"),
      tidyselect::matches("expect.*learn"),
      tidyselect::matches("why.*attend")
    ) |>
    tidyr::pivot_longer(
      -course_version,
      names_to = "questions",
      values_to = "responses"
    ) |>
    remove_newlines("responses") |>
    dplyr::arrange(course_version, questions, responses) |>
    join_original_column_names(id = id) |>
    tidyr::drop_na() |>
    dplyr::mutate(course_id = id) |>
    dplyr::relocate(course_id, course_version, questions, responses)
}

