
#' RStudio snippets to help with teaching
#'
#' @return Nothing. Used for the side effect of installing snippets for teaching.
#' @export
#'
install_snippets <- function() {
    snippets::install_snippets_from_package("r3admin")
    return(invisible(NULL))
}

