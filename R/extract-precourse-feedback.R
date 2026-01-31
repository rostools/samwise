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
      "workshop_id",
      "workshop_date",
      tidyselect::matches("worked.*well"),
      tidyselect::matches("could.*improved"),
      tidyselect::matches("describe.*problems"),
      tidyselect::matches("expect.*learn"),
      tidyselect::matches("why.*attend")
    ) |>
    tidyr::pivot_longer(
      -c(workshop_id, workshop_date),
      names_to = "question",
      values_to = "response"
    ) |>
    remove_newlines("response") |>
    dplyr::arrange("workshop_date", "question", "response") |>
    join_original_column_names() |>
    tidyr::drop_na() |>
    dplyr::relocate(
      "workshop_id",
      "workshop_date",
      "question",
      "response"
    ) |>
    drop_missing_responses()
}
