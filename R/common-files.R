#' Copy commonly used files for r-cubed courses.
#'
#' @param template Name of common file.
#'
#' @return Path to created common file.
#' @export
#'
copy_common_file <- function(file) {
  file <- rlang::arg_match(file, fs::path_file(list_common_files()))
  common_path <- fs::path_package("r3admin", "common", file)
  header_text <- glue::glue("# Automatically created by `r3admin::copy_common_file('{file}')` on {lubridate::today()}.")
  if (rprojroot::is_rstudio_project$testfun[[1]](".")) {
    readr::write_lines(
      x = c(header_text, readr::read_lines(common_path)),
      file = "_variables.yml"
    )
  } else {
    rlang::abort("You aren't in an R Project.")
  }
  return(fs::path_abs("_variables.yml"))
}

#' List files in the common directory.
#'
#' @param regexp Regular expression for the file path.
#'
#' @return Vector of file paths
#' @export
#'
#' @examples
#' list_common_files("LICENSE.md")
list_common_files <- function(regexp = NULL) {
  fs::path_package("r3admin", "common") |>
    fs::dir_ls(regexp = regexp)
}

#' List files in the template directory.
#'
#' @inheritParams list_common_files
#'
#' @return Vector of file paths.
#' @export
#'
#' @examples
#' list_template_files("planning-issue.md")
list_template_files <- function(regexp = NULL) {
  fs::path_package("r3admin", "templates") |>
    fs::dir_ls(regexp = regexp)
}

#' Read contents of a file in the common directory.
#'
#' @param file Name of file in the common directory.
#'
#' @return Character vector.
#' @export
#'
#' @examples
#' read_common("LICENSE.md")
read_common <- function(file) {
  file <- rlang::arg_match(file, fs::path_file(list_common_files()))
  readr::read_lines(list_common_files(file))
}

#' Read the contents of the R package installation instructions template fiile.
#'
#' @param course_type The type of course.
#'
#' @return Character vector.
#' @export
#'
#' @examples
#' read_template_pkg_install("intro")
read_template_pkg_install <- function(course_type = c("intro", "inter", "advanced")) {
  course_type <- rlang::arg_match(course_type)
  course_install_fn <- switch(
    course_type,
    intro = "install_packages_introduction()",
    inter = "install_packages_intermediate()",
    advanced = "install_packages_advanced()"
  )

  template_contents <- list_template_files("install-r-packages.md") |>
    readr::read_lines()
  whisker::whisker.render(
    template = template_contents,
    data = list(
      course_install_fn = course_install_fn
    )
  )
}
