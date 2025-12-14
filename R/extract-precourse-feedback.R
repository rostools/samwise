#' Extract and tidy up the pre-workshop feedback data.
#'
#' @inheritParams extract_participant_overview
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- get_preworkshop_survey("intro")
#' extract_preworkshop_feedback(survey)
#' }
extract_preworkshop_feedback <- function(data) {
  if (nrow(data) == 0) {
    cli::cli_warn("No data found in the pre-workshop survey.")
    return(NULL)
  }
  data |>
    anonymize_preworkshop() |>
    dplyr::select(
      .data$workshop_id,
      .data$workshop_date,
      tidyselect::matches("worked.*well"),
      tidyselect::matches("could.*improved"),
      tidyselect::matches("describe.*problems"),
      tidyselect::matches("expect.*learn"),
      tidyselect::matches("why.*attend")
    ) |>
    tidyr::pivot_longer(
      -c(.data$workshop_id, .data$workshop_date),
      names_to = "question",
      values_to = "response"
    ) |>
    remove_newlines("response") |>
    dplyr::arrange(.data$workshop_date, .data$question, .data$response) |>
    join_original_column_names(id = unique(data$workshop_id)) |>
    tidyr::drop_na() |>
    dplyr::relocate(
      .data$workshop_id,
      .data$workshop_date,
      .data$question,
      .data$response
    ) |>
    drop_missing_responses()
}
