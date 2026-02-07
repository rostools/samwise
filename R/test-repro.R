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
        rlang::catch_cnd(quarto::quarto_render(path))
    })
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
    filename = data_filename
  )
  fs::dir_ls(dir, glob = "*.Rproj", recurse = TRUE) |>
    fs::path_dir() |>
    purrr::walk(\(dir) move_zen_file_to_project(dir, data_filename)) |>
    purrr::walk(styler::style_dir) |>
    purrr::walk(render_project_qmds)
}
