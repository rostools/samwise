#' Create a GItHub organization for a course based on a naming pattern of the course's ID.
#'
#' @param id The identifier of the course based on [list_workshop_ids()].
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
  course_date <- get_upcoming_course_dates(id)[1]
  course_name <- get_course_repo(id)
  glue::glue("{course_name}-{stringr::str_sub(course_date, end = 7)}")
}
