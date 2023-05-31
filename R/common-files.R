#' Copy commonly used files for r-cubed courses.
#'
#' @param template Name of common file.
#'
#' @return Path to created `_variables.yml` file.
#' @export
#'
copy_common_file <- function(file = c("_variables.yml")) {
  template_path <- fs::path_package("r3admin", "common", file)
  header_text <- glue::glue("# Automatically created by `r3admin::copy_common_file('{file}')` on {lubridate::now()}.")
  if (rprojroot::is_rstudio_project$testfun[[1]](".")) {
    readr::write_lines(
      x = c(header_text, readr::read_lines(template_path)),
      file = "_variables.yml"
    )
  } else {
    rlang::abort("You aren't in an R Project.")
  }
  return(fs::path_abs("_variables.yml"))
}
