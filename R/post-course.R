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
#'
#' # create_course_tag(metadata$dates$advanced[1], "First version of the advanced course material for December 2022.")
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

  remote_host <- gert::git_remote_list() %>%
    dplyr::filter(.data$name == "origin") %>%
    dplyr::pull(url)

  remote_host_name <- remote_host %>%
    stringr::str_extract("(github|gitlab)")

  repo_name <- remote_host %>%
    stringr::str_extract("(github|gitlab)\\.com[:/](.*/.*)\\.git$", group = 2) %>%
    stringr::str_remove_all(":|\\.git")

  url_release <- switch(remote_host_name,
    github = glue::glue("https://github.com/{repo_name}/releases/new"),
    gitlab = glue::glue("https://gitlab.com/{repo_name}/-/releases/new")
  )

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
#' fs::path_package("r3admin", "extdata", "_people.csv") %>% zen_creators_from_csv()
zen_creators_from_csv <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols_only(
      full_name = readr::col_character(),
      primary_affiliation = readr::col_character(),
      orcid = readr::col_character(),
      contributor_role = readr::col_logical()
    )
  ) %>%
    dplyr::filter(contributor_role) %>%
    dplyr::mutate(
      family = stringr::word(full_name, -1),
      given = stringr::str_remove(full_name, glue::glue(" {family}"))
    ) %>%
    dplyr::transmute(
      name = glue::glue("{family}, {given}"),
      affiliation = primary_affiliation,
      orcid = orcid
    ) %>%
    dplyr::group_split(name) %>%
    purrr::map(as.list)
}

# as.array()

created_at_stamp <- function() {
  lubridate::stamp("# Created on March 10, 1999 at 20:10.", quiet = TRUE)(lubridate::now())
}

zen_title_from_desc <- function(path = ".") {
lesson_title <- desc::desc_get_field("Title", file = path) %>%
    stringr::str_remove_all("\"|\n") %>%
    stringr::str_replace_all(" +", " ") %>%
    stringr::str_replace(":", " -")
}

# create_zenodo_json <- function() {
#   list(
# upload_type = "lesson", access_right = "open",
# publication_date = course_date,
# title = zen_title_from_desc(path = path),
# creators = as.array(zen_creators_from_csv(path = path)),
# description = zen_description_from_desc(path = path),
#   )
# }

# description_content <- desc_get_field("Description") %>%
#     str_remove_all("\\n") %>%
#     str_replace_all(" +", " ")
#
# tag_archive_file <- str_c("r-cubed-intermediate-", repo_version, ".zip")
# git_archive_zip(tag_archive_file)
#
# zenodo <- ZenodoManager$new(
#     url = "https://zenodo.org/api",
#     logger = "INFO",
#     token = askpass::askpass()
# )
#
# update_record <- zenodo$getDepositionById("6513784")
# update_record <- zenodo$editRecord(update_record$id)
#
# # Only if new authors have been added.
# # TODO: Write filter to keep only new authors from Zenodo record.
# # pwalk(authors_df, update_record$addCreator)
# previous_tag <- git_tag_list()$name
# previous_tag <- tail(previous_tag, n = 2)[1]
# update_record$removeRelatedIdentifier(
#     "isIdenticalTo",
#     str_c("https://gitlab.com/rostools/r-cubed-intermediate/-/tags/", previous_tag)
# )
# update_record$addRelatedIdentifier(
#     "isIdenticalTo",
#     str_c("https://gitlab.com/rostools/r-cubed-intermediate/-/tags/", repo_version)
# )
#
# update_record$setPublicationDate(full_course_date)
# update_record$setVersion(repo_version)
# deposited_record <- zenodo$depositRecordVersion(update_record, files = tag_archive_file)
# fs::file_delete(tag_archive_file)

# library(tidyverse)
#
# text <- c("(13 Male, 12 F)", "(100 Men, 13 F)", "(21 F, 534 M)", "nothing")
# tibble(
#   men = text %>%
#     str_extract("\\(?([[:digit:]]+) M(ales?|[ea]n)?[,\\)]", group = 1),
#   women = text %>%
#     str_extract(",? ?([[:digit:]]+) (Females?|Wom[ae]n|F|W)[\\),]", group = 1)
# )
