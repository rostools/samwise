#' Create a GitHub organization for a workshop.
#'
#' @param id The identifier of the workshop based on [list_workshop_ids()].
#'
#' @returns A character scalar with the name of the organization.
#' @export
#'
create_github_org <- function(id) {
  org_name <- create_github_org_name(id)

  cli::cli_inform(c(
    "Create a GitHub organization with this name: {.val {org_name}} ",
    "*" = "Go to {.href https://github.com/account/organizations/new?plan=free}"
  ))

  continue <- yesno::yesno2(
    "\nHave you finished creating the GitHub organization?"
  )
  if (!continue) {
    cli::cli_abort("Organization wasn't created, stopping the function.")
  }

  ghclass::org_set_repo_permission(org_name, repo_permission = "none")
  org_name
}

create_github_org_name <- function(id) {
  date <- get_upcoming_workshop_dates(id)[1]
  name <- get_workshop_repo(id)
  glue::glue("{name}-{stringr::str_sub(date, end = 7)}")
}
