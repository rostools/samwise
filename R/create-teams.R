
#' Create randomly generated group names.
#'
#' Set a seed before hand to fix the names generated.
#'
#' @param max_name_length Longest characters that the group name should be.
#'
#' @return A character vector of names.
#' @export
#'
#' @examples
#' create_group_names(20)
create_group_names <- function(max_name_length = 18) {
  name_prefix <- tibble::tibble(adjective = praise::praise_parts$adjective) %>%
    dplyr::filter(nchar(.data$adjective) <= (max_name_length / 2)) %>%
    dplyr::pull(.data$adjective) %>%
    stringr::str_to_sentence() %>%
    sample()

  name_suffix <- tidytext::parts_of_speech %>%
    dplyr::filter(
      stringr::str_detect(.data$pos, "^Noun$"),
      nchar(.data$word) <= (max_name_length / 2),
      !stringr::str_detect(.data$word, "\\d|/|-")
    ) %>%
    dplyr::sample_n(length(name_prefix)) %>%
    dplyr::pull(.data$word) %>%
    stringr::str_to_sentence()

  stringr::str_c(name_prefix, name_suffix)
}

group_names_to_one_pdf <- function(group_names, output_dir = here::here("_ignore/group-names")) {
  group_names %>%
    purrr::walk(group_name_to_pdf, output_dir = output_dir)
  single_files <- fs::dir_ls(output_dir, glob = "*.pdf")
  pdftools::pdf_combine(single_files, output = fs::path(output_dir, "_all-groups.pdf"))
  fs::file_delete(single_files)
}

group_name_to_pdf <- function(group_name, output_dir) {
  withr::local_dir(output_dir)
  quarto::quarto_render(
    input = fs::path_package("r3admin", "templates", "group-name-pdf.qmd"),
    output_file = fs::path_ext_set(group_name, "pdf"),
    execute_params = list(
      name = group_name
    )
  )
}

group_names_to_html_table <- function(group_name, number_participants, output_dir) {
  withr::local_dir(output_dir)
  quarto::quarto_render(
    input = fs::path_package("r3admin", "templates", "group-name-strips.qmd"),
    output_file = fs::path_ext_set(group_name, "html"),
    execute_params = list(
      name = group_name,
      number_participants = number_participants
    )
  )
}

group_names_as_strips_html <- function(group_names,
                                      number_participants,
                                      output_dir = here::here("_ignore/group-names")) {
  group_names %>%
    purrr::walk(group_names_to_html_table,
                output_dir = output_dir,
                number_participants = number_participants)
  # fs::dir_ls(output_dir, glob = "*.html") %>%
  #   purrr::walk(webshot2::webshot) %>%
  #   fs::file_delete()
}


#' Assign learners into groups.
#'
#' @param data Survey data.
#' @param group_names Character vector that has the names to group into.
#' @param score_cutoff Point at which to split persons into "low" vs "high" skill (completely arbitrary).
#'
#' @return A data frame with team, name, and skill score.
#' @export
#'
assign_learners_to_groups <- function(data, group_names, score_cutoff = 3) {
  data %>%
    dplyr::select(.data$full_name, tidyselect::matches("^perceived")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(perceived_skill_score = sum(dplyr::c_across(tidyselect::starts_with("perceived")))) %>%
    dplyr::mutate(team = (.data$perceived_skill_score >= score_cutoff) %>%
      randomizr::block_ra(conditions = group_names) %>%
      as.character()) %>%
    dplyr::select(.data$team, .data$full_name, .data$perceived_skill_score) %>%
    dplyr::arrange(.data$team, .data$perceived_skill_score)
}

assign_instructors_to_groups <- function(group_names, instructors) {
  tibble::tibble(
    teams = group_names,
    primary = sample(rep(instructors, times = 2), 6),
    secondary = sample(rep(instructors, times = 2), 6)
  )
}
# instructor_assigned_teams
#

# gh_teams_prep <- presurvey_tidy %>%
#   select(full_name, github_username, matches("^perceived_.*_updated$")) %>%
#   mutate(across(-c(full_name, github_username), ~ as.numeric(.x) > 1)) %>%
#   mutate(
#     perceived_skill_score =
#       perceived_skill_r_updated + perceived_skill_data_analysis_updated +
#         perceived_skill_programming_updated + perceived_skill_git_updated
#   ) %>%
#   select(full_name, github_username, perceived_skill_score)
