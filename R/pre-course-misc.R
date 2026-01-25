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
create_team_project <- function(
  repo_path,
  clone_directory = fs::path("~", "Desktop")
) {
  project_folder <- fs::path(clone_directory, repo_path)
  ghclass::local_repo_clone(
    repo_path,
    fs::path_dir(project_folder)
  )
  usethis::local_project(project_folder)
  rlang::catch_cnd(fs::file_delete(fs::path(project_folder, ".gitignore")))
  prodigenr::setup_project(project_folder)
  usethis::use_git_ignore("data/")
  readr::read_lines("DESCRIPTION") |>
    # To avoid merge conflicts, so set a fixed ProjectId
    append("ProjectId: 7144761b-281f-458f-b7bf-44fd33c944ea") |>
    readr::write_lines("DESCRIPTION")
  gert::git_status()$file |>
    gert::git_add()
  gert::git_commit("Setup project")
  gert::git_push()
}

setup_team_repos <- function(gh_org) {
  team_repos <- ghclass::org_repos(gh_org)
  team_repos |>
    purrr::walk(create_team_project)
}

clone_team_repos <- function(gh_org) {
  team_repos <- ghclass::org_repos(gh_org)
  team_repos |>
    purrr::walk(clone_project_repo)
}

clone_project_repo <- function(
  repo_path,
  clone_directory = fs::path("~", "Desktop")
) {
  project_folder <- fs::path(clone_directory, repo_path)
  ghclass::local_repo_clone(
    repo_path,
    fs::path_dir(project_folder)
  )
}

pull_project_repo <- function(
  repo_path,
  local_directory = fs::path("~", "Desktop")
) {
  project_folder <- fs::path(local_directory, repo_path)
  ghclass::local_repo_pull(
    project_folder
  )
}

pull_team_repos <- function(gh_org) {
  team_repos <- ghclass::org_repos(gh_org)
  team_repos |>
    purrr::walk(pull_project_repo)
}

render_team_qmds <- function(
  gh_org,
  local_directory = fs::path("~", "Desktop")
) {
  team_repos <- ghclass::org_repos(gh_org)
  qmd_path <- fs::path(
    local_directory,
    team_repos,
    "docs",
    "report.qmd"
  )
  qmd_path |>
    purrr::walk(
      ~ {
        cli::cli_inform("Using {.val {.x}}")
        quarto::quarto_render(.x, quiet = TRUE)
      }
    )
}

setup_team_projects <- function(
  data,
  organization
) {
  checkmate::check_data_frame(data)
  checkmate::check_names(
    data,
    permutation.of = c("username", "team_names")
  )

  # Invite to the org
  ghclass::org_invite(organization, data$username)

  # Create the teams
  ghclass::team_create(
    organization,
    unique(data$team_names)
  )

  # Invite users to the team
  ghclass::team_invite(
    organization,
    data$username,
    data$team_names
  )

  gh_repos <- ghclass::repo_create(organization, data$team_names)
  gh_repos <- ghclass::org_repos(organization)
  ghclass::repo_add_team(sort(gh_repos), sort(unique(data$team_names)))
  setup_team_repos(organization)
}
