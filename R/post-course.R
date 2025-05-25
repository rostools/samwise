# Tag and release on GitLab -----------------------------------------------

#' Create a tag after finishing a course and release on GitHub/GitLab
#'
#' @param start_date The date the course starts.
#' @param message A message to add to the git tag.
#'
#' @return Nothing. Used for the side effect of adding a git tag and opening
#'   the URL to make a release.
#' @export
#'
#' @examples
#' \dontrun{
#' # create_course_tag(get_course_dates("adv")[1], "First version of the advanced course material for December 2022.")
#' }
create_course_tag <- function(start_date, message) {
  checkmate::assert_character(start_date)
  checkmate::assert_character(message)

  desc_repo_version <- stringr::str_replace_all(start_date, "-", ".")
  repo_version <- stringr::str_c("v", desc_repo_version)

  desc::desc_set_version(desc_repo_version)
  version_tag <- gert::git_tag_create(
    name = repo_version,
    message = message
  )

  gert::git_push()
  gert::git_tag_push(repo_version)

  remote_host <- gert::git_remote_list() |>
    dplyr::filter(name == "origin") |>
    dplyr::pull(url)

  remote_host_name <- remote_host |>
    stringr::str_extract("github")

  repo_name <- remote_host |>
    stringr::str_extract("github\\.com[:/](.*/.*)\\.git$", group = 1) |>
    stringr::str_remove_all(":|\\.git")

  url_release <- glue::glue("https://github.com/{repo_name}/releases/new")

  if (interactive()) {
    browseURL(url_release)
  }
}

# Update Zenodo -----------------------------------------------------------

#' Convert CSV of instructors/contributors to a list (in prep for Zenodo).
#'
#' @param path Path to CSV with contributors/instructors.
#'
#' @return A list
#' @noRd
#'
#' @examples
#' \dontrun{
#' fs::path_package("samwise", "extdata", "_people.csv") |> zen_creators_from_csv()
#' }
zen_creators_from_csv <- function(path) {
  # TODO: Need to update this.
  readr::read_csv(
    path,
    col_types = readr::cols_only(
      full_name = readr::col_character(),
      primary_affiliation = readr::col_character(),
      orcid = readr::col_character(),
      contributor_role = readr::col_logical()
    )
  ) |>
    dplyr::filter(contributor_role) |>
    dplyr::mutate(
      family = stringr::word(full_name, -1),
      given = stringr::str_remove(full_name, glue::glue(" {family}"))
    ) |>
    dplyr::transmute(
      name = glue::glue("{family}, {given}"),
      affiliation = primary_affiliation,
      orcid = orcid
    ) |>
    dplyr::group_split(name) |>
    purrr::map(as.list)
}

created_at_stamp <- function() {
  lubridate::stamp(
    "# Created on March 10, 1999 at 20:10.",
    quiet = TRUE
  )(lubridate::now())
}
