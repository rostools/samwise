
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

# copy_instructor_groups_for_gh_issue <-
# instructor_assigned_teams %>%
#     rename_with(str_to_sentence) %>%
#     knitr::kable() %>%
#     clipr::write_clip()

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

#   create_team_projects() <- function(repo_path, parent_directory) {
#   project_folder <- fs::path(parent_directory, repo_path)
#   ghclass::local_repo_clone(
#     repo_path,
#     fs::path_dir(project_folder)
#   )
#   prodigenr::setup_project(project_folder)
#   withr::local_dir(project_folder)
#   usethis::use_blank_slate("project")
#   usethis::use_data_raw("original-data", open = FALSE)
#   gert::git_status()$file %>%
#     gert::git_add()
#   gert::git_commit("Setup project")
#   gert::git_push()
# }
#
# course_team_repos <- org_repos(org_gh_course_name)
# fs::dir_create(fs::path("~", "Desktop", org_gh_course_name))
# course_team_repos %>%
#   walk(create_team_projects,
#     parent_directory = fs::path("~", "Desktop")
#   )
