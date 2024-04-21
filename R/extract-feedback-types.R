#' Extract the quantitative responses from the feedback survey.
#'
#' @name extract_feedback
#' @rdname extract_feedback
#' @param data The feedback data.
#'
#' @return A tibble.
#'
#' @examples
#' \dontrun{
#' intro_feedback <- get_feedback_survey("intro")
#' general_feedback <- get_feedback_survey("general")
#'
#' extract_feedback_quantitative(intro_feedback)
#' extract_feedback_quantitative(general_feedback)
#' extract_feedback_overall(intro_feedback)
#' extract_feedback_overall(general_feedback)
#' extract_feedback_sessions(intro_feedback)
#' extract_feedback_sessions(general_feedback)
#' }
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
    dplyr::count(course_id, course_date, statement, response) |>
    dplyr::arrange(course_id, course_date)
}

#' @describeIn extract_feedback Extract and tidy up the overall comments
#'   from the feedback survey data.
#' @export
extract_feedback_overall <- function(data) {
  data %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "^day$", "session_name")) |>
    dplyr::filter(
      session_name %in% c("Day 3", "End of course"),
      stringr::str_detect(
        question,
        ".*any other (general )?(comments or feedback|feedback or comments).*"
      )
    ) %>%
    dplyr::select(-question, -session_name, -dplyr::contains("course_name")) %>%
    dplyr::filter(stringr::str_detect(
      response,
      "^No$",
      negate = TRUE
    ))
}

#' @describeIn extract_feedback Extract and tidy up the session specific
#'   comments from the feedback survey data.
#' @export
extract_feedback_sessions <- function(data) {
  data %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "^day$", "session_name")) |>
    dplyr::filter(stringr::str_detect(question,
      "^(What could be improved|What worked well).*"
    )) %>%
    dplyr::mutate(question = question %>%
      stringr::str_remove_all("What|(this )?session|\"|\\?") %>%
      stringr::str_trim()) %>%
    # Drop any duplicate comments (like repeats of "great!")
    dplyr::distinct()
}
