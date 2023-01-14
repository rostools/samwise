
#' Paste emails from survey to send invites to join Slack.
#'
#' @param emails List of emails to format.
#'
#' @return Used for the side effect of pasting to the clipboard.
#' @export
#'
copy_emails_for_slack_invite <- function(emails) {
  emails %>%
    stringr::str_c(collapse = ", ") %>%
    clipr::write_clip()
}

#' Copy to clipboard the team-assigned instructors data as a Markdown table.
#'
#' @param data The data that contains the team names and the assigned
#'   instructors.
#' @param gh_org The name of the course's GitHub organizaton, usually in the
#'   form of `NAME-YYYY-MM`.
#'
#' @return Used for side effect of copying Markdown table to clipboard.
#' @export
#'
copy_instructors_to_groups_table <- function(data, gh_org) {
  data %>%
    dplyr::arrange(.data$team) %>%
    dplyr::mutate(team = glue::glue("[{team}](https://github.com/{gh_org}/{team})")) %>%
    dplyr::rename_with(stringr::str_to_sentence) %>%
    knitr::kable() %>%
    clipr::write_clip()
}

copy_tidy_names_by_team <- function(data) {
  data %>%
    dplyr::select(.data$team, .data$full_name) %>%
    dplyr::group_split(team) %>%
    dplyr::pmap_chr(formatted_names_by_team) %>%
    stringr::str_c(collapse = "\n\n") %>%
    clipr::write_clip()
}

# Format teams and names so its easier to put name tags when physically
# putting groups together.
formatted_names_by_team <- function(data) {
  append(
    paste0("# ", unique(data$team), "\n"),
    data$full_name
  ) %>%
    stringr::str_c(collapse = "\n- ")
}

# Setup project and other settings for teams ------------------------------

#' Clone a GitHub repo, setup the project with prodigenr, then commit and push.
#'
#' This function is used as a helper for `setup_team_repos()`.
#'
#' @param repo_path The GitHub style repo path (`orgname/reponame`).
#' @param clone_directory Where to clone the repository.
#'
#' @return Used for the side effects of creating the project, committing, and
#'   pushing.
#' @export
#'
create_team_project <- function(repo_path,
                                clone_directory = fs::path("~", "Desktop")) {
  project_folder <- fs::path(clone_directory, repo_path)
  ghclass::local_repo_clone(
    repo_path,
    fs::path_dir(project_folder)
  )
  prodigenr::setup_project(project_folder)
  withr::local_dir(project_folder)
  usethis::use_blank_slate("project")
  usethis::use_data_raw("original-data", open = FALSE)
  gert::git_status()$file %>%
    gert::git_add()
  gert::git_commit("Setup project")
  gert::git_push()
}

#' Setup all team repositories to be ready for the assignment.
#'
#' @param gh_org The name of the course's GitHub organizaton, usually in the
#'   form of `NAME-YYYY-MM`.
#'
#' @return Used for the side effect of selecting on all repos and setting up
#'   projects for them.
#' @export
#'
setup_team_repos <- function(gh_org) {
  course_team_repos <- ghclass::org_repos(gh_org)
  fs::dir_create(fs::path("~", "Desktop", gh_org))
  course_team_repos %>%
    purrr::walk(create_team_project,
      clone_directory = fs::path("~", "Desktop")
    )
}
