download_zen_file_to_downloads <- function(url, path) {
  download.file(
    url = url,
    destfile = fs::path(fs::path_home(), "Downloads", path)
  )
}

download_zenodo_data <- function(path) {
  if (!fs::file_exists(fs::path(path, "data", data_file))) {
    download.file(
      url = "https://zenodo.org/records/8292712/files/SUA_CVDs_risk_factors.csv",
      destfile = fs::path(path, "data", "SUA_CVDs_risk_factors.csv")
    )
  }
}

render_project_qmds <- function(dir) {
  qmd_paths <- fs::dir_ls(
    fs::path(dir, "docs"),
    glob = "*.qmd"
  )
  qmd_paths |>
    purrr::compact() |>
    purrr::walk(
      \(path) {
        cli::cli_inform("Rendering {path}")
        # To allow rendering to continue even if there is an error with one.
        rendered <- purrr::safely(quarto::quarto_render)(path, quiet = TRUE)
        if (!is.null(rendered$error)) {
          cli::cli_warn("Couldn't render {path}")
        } else if (!is.null(rendered$result)) {
          cli::cli_inform("Rendered {path}")
        }
      }
    )
}

move_zen_file_to_project <- function(dir, path) {
  fs::dir_create(fs::path(dir, "data"))
  if (!fs::file_exists(fs::path(dir, "data", path))) {
    fs::file_copy(
      from = fs::path(fs::path_home(), "Downloads", path),
      to = fs::path(dir, "data", path)
    )
  }
}

test_repro_team_projects <- function(dir = fs::path("~", "Desktop")) {
  data_filename <- "cvd_risk_factors.csv"
  download_zen_file_to_downloads(
    url = "https://zenodo.org/records/8292712/files/SUA_CVDs_risk_factors.csv",
    path = data_filename
  )
  fs::dir_ls(dir, glob = "*.Rproj", recurse = TRUE) |>
    fs::path_dir() |>
    purrr::walk(\(dir) move_zen_file_to_project(dir, data_filename)) |>
    purrr::walk(styler::style_dir) |>
    purrr::walk(render_project_qmds)
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

list_deps <- function(path) {
  desc::desc_get_deps(path)$package |>
    stringr::str_subset("^R$", negate = TRUE) |>
    unique()
}

list_repos_deps <- function(dir = fs::path("~/Desktop")) {
  fs::dir_ls(dir, recurse = TRUE, type = "file", regexp = "DESCRIPTION") |>
    purrr::map(list_deps) |>
    purrr::flatten() |>
    unlist() |>
    unique()
}
