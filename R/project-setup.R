#' Create the team project for the intermediate workshop
#'
#' @param path Path to where the project should be created. It will overwrite
#'   any existing project.
#'
#' @returns Invisibly returns the path to the created project.
#'
#' @export
#' @examples
#' \dontrun{
#' create_inter_project_repo("~/Desktop/dime-intermediate")
#' }
create_inter_project_repo <- function(path) {
  if (fs::dir_exists(path)) {
    rlang::catch_cnd(fs::file_delete(fs::path(path, ".gitignore")))
  }
  prodigenr::setup_project(path)
  usethis::local_project(path, quiet = TRUE)
  usethis::use_git_ignore(c(
    "data/*.csv",
    "data-raw/*.csv",
    "*.zip",
    "*.tar",
    "*.html",
    "*_files"
  ))
  fs::file_copy(
    fs::path_package("samwise", "templates", "dime-intermediate.R"),
    fs::path("data-raw", "dime.R")
  )
  # Intermediate only
  fs::file_create(fs::path("R", "functions-1.R"))
  fs::file_create(fs::path("R", "functions-2.R"))
  # TODO: Create Quarto from here?
  readr::read_lines("DESCRIPTION") |>
    # To avoid merge conflicts, so set a fixed ProjectId
    append("ProjectId: 7144761b-281f-458f-b7bf-44fd33c944ea") |>
    readr::write_lines("DESCRIPTION")
  invisible(path)
}

create_github_team_repos <- function(
  team_names,
  organization
) {
  team_names <- unique(team_names)
  ghclass::repo_create(organization, team_names)
  Sys.sleep(2)

  ghclass::repo_add_team(glue::glue("{organization}/{team_names}"), team_names)
  Sys.sleep(2)

  setup_team_repos(organization)
}

setup_team_repos <- function(gh_org) {
  team_repos <- ghclass::org_repos(gh_org)
  team_repos |>
    purrr::walk(setup_team_project_repo)
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
setup_team_project_repo <- function(
  repo_path,
  clone_directory = fs::path("~", "Desktop")
) {
  project_folder <- fs::path(clone_directory, repo_path)
  ghclass::local_repo_clone(
    repo_path,
    fs::path_dir(project_folder)
  )
  usethis::local_project(project_folder, quiet = TRUE)
  # TODO: Update to use for intro, advanced.
  create_inter_project_repo(project_folder)
  gert::git_status()$file |>
    gert::git_add()
  gert::git_commit("Setup project")
  gert::git_push()
}
