#' Plot the quantitative feedback from the survey data
#'
#' @param data The quantitative feedback data, as returned by [read_surveys()].
#'
#' @returns A [ggplot2::ggplot] object.
#' @export
#'
plot_feedback <- function(data) {
  data |>
    dplyr::mutate(
      statement = stringr::str_remove(
        .data$statement,
        "^Please .* (workshop|course). "
      ),
      response = clean_response(.data$response)
    ) |>
    ggplot2::ggplot(ggplot2::aes(y = response, x = n, fill = response)) +
    ggplot2::geom_col(show.legend = TRUE) +
    ggplot2::facet_wrap(
      ggplot2::vars(statement),
      ncol = 3,
      labeller = ggplot2::label_wrap_gen(width = 40)
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Number of responses") +
    ggplot2::guides(fill = ggplot2::guide_legend("Responses")) +
    ggplot2::scale_fill_manual(
      breaks = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"),
      values = viridis::viridis(5, option = "D"),
      drop = FALSE
    )
}

#' Plot an overview of who the learners are.
#'
#' @param data The pre-workshop survey data as returned by [read_surveys()].
#'
#' @inherit plot_feedback return
#' @export
#'
plot_overview <- function(data) {
  data |>
    dplyr::filter(stringr::str_detect(
      .data$question,
      "[cC]ode of [cC]onduct",
      negate = TRUE
    )) |>
    dplyr::mutate(question = dplyr::case_when(
      stringr::str_detect(question, "pronoun") ~ "Preferred pronoun",
      stringr::str_detect(question, "position") ~ "Formal position",
      TRUE ~ question
    )) |>
    dplyr::mutate(
      response = dplyr::if_else(
        stringr::str_detect(.data$question, "position"),
        clean_positions(.data$response),
        .data$response
      ),
      response = dplyr::if_else(
        stringr::str_detect(.data$question, "pronoun"),
        clean_pronouns(.data$response),
        .data$response
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = count, y = response, fill = response)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(ggplot2::vars(question), ncol = 3, scales = "free_y") +
    ggplot2::scale_fill_viridis_d(begin = 0.5, end = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::labs(x = "Number of responses")
}

# Cleaners ----------------------------------------------------------------

clean_response <- function(x) {
  x <- forcats::as_factor(na.omit(x))
  expected_levels <- c(
    "Strongly agree",
    "Agree",
    "Neutral",
    "Disagree",
    "Strongly disagree"
  )
  actual_levels <- levels(x)

  x |>
    forcats::fct_relevel(dplyr::intersect(expected_levels, actual_levels)) |>
    forcats::fct_expand(expected_levels) |>
    forcats::fct_rev()
}

clean_positions <- function(x) {
  x |>
    stringr::str_to_sentence() |>
    stringr::str_replace("^.*[cC]onsult[ea]nt.*$", "Consultant") |>
    stringr::str_replace("[Dd]ata ?manager", "Data manager") |>
    stringr::str_replace("assistent", "assistant") |>
    stringr::str_replace("Projectleader", "Project leader") |>
    stringr::str_replace("Phd", "PhD")
}

clean_pronouns <- function(x) {
  dplyr::if_else(
    stringr::str_detect(x, "([hH]e/[hH]im)|([sS]he/[hH]er)"),
    x,
    "Other/unspecified"
  )
}
