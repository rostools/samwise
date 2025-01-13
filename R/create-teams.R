#' Assign learners into groups.
#'
#' @param data Precourse survey data.
#' @param group_names Character vector that has the names to group into.
#' @param score_cutoff Point at which to split persons into "low" vs "high" skill (completely arbitrary).
#'
#' @return A [tibble::tibble].
#' @export
#'
assign_learners_to_groups <- function(data, group_names, score_cutoff = 3) {
  data %>%
    dplyr::select(
      full_name,
      tidyselect::contains("github"),
      tidyselect::matches("^perceived")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("perceived"), as.numeric)) %>%
    dplyr::mutate(perceived_skill_score = sum(dplyr::c_across(tidyselect::starts_with("perceived")))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(team = (perceived_skill_score >= score_cutoff) %>%
      randomizr::block_ra(conditions = group_names) %>%
      as.character()) %>%
    dplyr::select(
      team,
      full_name,
      tidyselect::contains("github"),
      perceived_skill_score
    ) %>%
    dplyr::arrange(team, perceived_skill_score)
}

assign_instructors_to_groups <- function(group_names, instructors) {
  if (length(instructors) != length(instructors)) {
    instructors <- rep(instructors, times = 2)
  }
  tibble::tibble(
    team = group_names,
    instructor = sample(instructors, length(group_names))
  )
}
