#' Create randomly generated group names based on city coordinating from what3words.
#'
#' From all cities globally, get the [what3words](https://what3words.com/)
#' coordinates to use as fun group names.
#'
#' @param number_groups The number of group names to create.
#'
#' @return A character vector of names.
#' @export
#'
#' @examples
#' create_group_names(20)
create_group_names <- function(number_groups) {
  if (Sys.getenv("WTW_API_KEY") == "") {
    cli::cli_abort("You need to add the WTW API key in the {.val .Renviron} file to use this function.")
  }
  random_cities <- maps::world.cities |>
    tibble::as_tibble() |>
    dplyr::sample_n(number_groups)

  whatthreewords::words_from_coords(
    lat = random_cities$lat,
    lon = random_cities$long
  )
}

group_names_to_one_pdf <- function(group_names, output_dir = here::here("_ignore/group-names")) {
  group_names %>%
    purrr::walk(group_name_to_pdf, output_dir = output_dir)
  single_files <- fs::dir_ls(output_dir, glob = "*.pdf")
  combined_pdf_file <- fs::path(output_dir, "_all-groups.pdf")
  Sys.sleep(1)
  pdftools::pdf_combine(single_files, output = combined_pdf_file)
  if (!fs::file_exists(combined_pdf_file)) {
    cli::cli_warn("The file {.path {fs::path_file(combined_pdf_file)}} wasn't actually created, try again?")
  } else {
    cli::cli_alert_success("The group names PDF file ({.path {fs::path_file(combined_pdf_file)}}) was created!")
  }
  fs::file_delete(single_files)
}

group_name_to_pdf <- function(group_name, output_dir) {
  withr::local_dir(output_dir)
  output_file <- stringr::str_replace_all(group_name, "\\.", "-")
  output_file <- fs::path_ext_set(output_file, "pdf")
  quarto::quarto_render(
    input = fs::path_package("r3admin", "templates", "group-name-pdf.qmd"),
    output_file = output_file,
    execute_params = list(
      team_name = group_name
    )
  )
}

teams_with_members_to_one_pdf <- function(team_names_with_members, output_dir = here::here("_ignore/group-names")) {
  lifecycle::deprecate_stop("Now", "teams_with_members_to_one_pdf()")
  team_names_with_members %>%
    dplyr::group_split(team_name) %>%
    purrr::walk(~ {
      teams_with_members_to_pdf(
        team_name = unique(.x$team_name),
        team_members = .x$team_members,
        output_dir = output_dir
      )
    })
  single_files <- fs::dir_ls(output_dir, glob = "*.pdf")
  pdftools::pdf_combine(single_files, output = fs::path(output_dir, "_all-groups.pdf"))
  fs::file_delete(single_files)
}

teams_with_members_to_pdf <- function(team_name, team_members, output_dir) {
  withr::local_dir(output_dir)
  quarto::quarto_render(
    input = fs::path_package("r3admin", "templates", "teams-with-members-pdf.qmd"),
    output_file = fs::path_ext_set(team_name, "pdf"),
    execute_params = list(
      team_name = team_name,
      team_members = team_members
    )
  )
}

group_names_as_strips_html <-
  function(group_names,
           number_participants,
           output_dir = here::here("_ignore/group-names")) {
    fs::dir_create(output_dir)
    withr::local_dir(output_dir)
    quarto::quarto_render(
      input = fs::path_package("r3admin", "templates", "group-name-strips.qmd"),
      output_file = "group-names-to-cut.html",
      execute_params = list(
        name = group_names,
        number_participants = number_participants
      )
    )
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
    dplyr::select(
      full_name,
      tidyselect::any_of("github_username"),
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
      tidyselect::any_of("github_username"),
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
