# html_files <- dir_ls(path = "public",
#                      glob = "*.html",
#                      recurse = TRUE)
# stop("To prevent accidental sourcing.")
#
# # Test URL is active or alive ---------------------------------------------
#
# # URLs change over time or get removed from the internet. This code
# # runs gets all http(s) links and does a simple "GET" to see if it
# # is active or not. It isn't always correct, but its a good starting
# # point.
#
# get_href_links <- function(x) {
#     x %>%
#         read_html() %>%
#         html_nodes("a") %>%
#         html_attr("href")
# }
#
# url_links <- map(html_files, get_href_links) %>%
#     flatten_chr() %>%
#     str_subset("^https?.*$") %>%
#     str_subset(".*\\.(exe|zip)$", negate = TRUE) %>%
#     str_remove("%3E") %>%
#     unique()
#
# bad_url <- function(x) {
#     bad_url <- NA_character_
#     if (http_error(x))
#         bad_url <- x
#     bad_url
# }
#
# url_tested <- map_chr(url_links, bad_url) %>%
#     na.omit()
#
# url_tested

# TODO: check if all images are used. Check directly from md and include_graphics?

check_duplicate_timestamps <- function(data) {
  if (any(duplicated(data$timestamp))) {
    rlang::abort("There are duplicate timestamps, please investigate.")
  }
  data
}

# Check who hasn't finished the survey ------------------------------------

check_who_not_finish_survey <- function(data, participant_list) {
  data %>%
    dplyr::select(.data$full_name, .data$email) %>%
    dplyr::mutate(name_from_survey = .data$full_name,
                  email_from_survey = .data$email) %>%
    dplyr::full_join(
      participant_list %>%
        dplyr::select(.data$email) %>%
        dplyr::mutate(email_from_list = .data$email),
      by = "email"
    ) %>%
    dplyr::select(full_name, contains("name"), contains("email"))
}

copy_emails_for_reminder <- function(data) {
  data %>%
    dplyr::filter(is.na(name_from_survey)) %>%
    dplyr::pull(email_from_list) %>%
    clipr::write_clip()
}

copy_reminder_email_text <- function() {
  fs::path_package("r3admin", "templates", "email-reminder.txt") %>%
    readr::read_lines() %>%
    clipr::write_clip()
}

# Check who has problems --------------------------------------------------

check_precourse_problems <- function(data) {
  data %>%
    dplyr::filter(.data$encounter_problems == "Yes") %>%
    dplyr::select(tidyselect::any_of(c(
      "full_name",
      "email",
      "check_setup_output",
      "check_project_setup_output",
      "describe_problems",
      "when_available_for_help"
    )))
}

copy_names_with_problems <- function(full_name) {
  full_name %>%
    stringr::str_c("- ", ., collapse = "\n") %>%
    append("People with problems:", after = 0) %>%
    stringr::str_c(collapse = " \n") %>%
    clipr::write_clip()
}

# # Who to still invite (those that finished pre-course tasks later).
# currently_invited <- c(
#     org_members(org_gh_course_name) %>%
#         str_subset("lwjohnst86", negate = TRUE) %>%
#         str_to_lower(),
#     str_to_lower(org_pending(org_gh_course_name))
# )
# need_to_invite <- gh_teams_prep$github_username %>%
#     str_to_lower() %>%
#     setdiff(currently_invited)

# Setup checks ------------------------------------------------------------

pretty_code_checks <- function(name, email, check) {
  glue::glue("\n#### {unique(name)}  ({unique(email)})\n\n```\n{check}\n```\n\n")
}

pretty_text_checks <- function(name, email, check) {
  glue::glue("\n#### {unique(name)} ({unique(email)})\n\n{check}\n\n")
}

check_setup <- function(data) {
  data %>%
    dplyr::group_split(.data$full_name) %>%
    purrr::map_chr(~pretty_code_checks(
      .x$full_name,
      .x$email,
      .x$check_setup_output
      )
    ) %>%
    stringr::str_c(collapse = "\n\n")
}

check_project_setup <- function(data) {
  data %>%
    dplyr::group_split(.data$full_name) %>%
    purrr::map_chr(~pretty_code_checks(
      .x$full_name,
      .x$email,
      .x$check_setup_output
      )
    ) %>%
    stringr::str_c(collapse = "\n\n\n")
}

check_problem_description <- function(data) {
  data %>%
    dplyr::group_split(.data$full_name) %>%
    purrr::map_chr(~pretty_text_checks(
      .x$full_name,
      .x$email,
      paste0(.x$describe_problems, "\n\n", .x$when_available_for_help)
      )
    ) %>%
    stringr::str_c(collapse = "\n\n")
}
