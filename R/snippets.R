#' RStudio snippets to help with teaching (in dev.)
#'
#' @return Nothing. Used for the side effect of installing snippets for teaching.
#' @export
#'
install_snippets <- function() {
  warning("In development")
  return(invisible(NULL))
}

#' Copy snippets from package to clipboard.
#'
#' @return Paste snippets.
#' @export
#'
copy_snippets <- function() {
  fs::path_package("r3admin", "snippets", "markdown.snippets") |>
    readr::read_lines() |>
    clipr::write_clip()
}
