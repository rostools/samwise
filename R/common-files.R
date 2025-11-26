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
  fs::path_package("samwise", "templates") |>
    fs::dir_ls(regexp = regexp)
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
read_template_pkg_install <- function(
  course_type = c("intro", "inter", "advanced")
) {
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
