# Check who hasn't finished the survey ------------------------------------

check_who_not_finish_survey <- function(data, participant_list) {
  data %>%
    dplyr::select(full_name, email) %>%
    dplyr::mutate(
      name_from_survey = full_name,
      email_from_survey = email
    ) %>%
    dplyr::full_join(
      participant_list %>%
        dplyr::select(email) %>%
        dplyr::mutate(email_from_list = email),
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
    dplyr::filter(encounter_problems == "Yes") %>%
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

# Setup checks ------------------------------------------------------------

pretty_code_checks <- function(name, email, check) {
  glue::glue("\n#### {unique(name)}  ({unique(email)})\n\n```\n{check}\n```\n\n")
}

pretty_text_checks <- function(name, email, check) {
  glue::glue("\n#### {unique(name)} ({unique(email)})\n\n{check}\n\n")
}

check_setup <- function(data) {
  data %>%
    dplyr::group_split(full_name) %>%
    purrr::map_chr(~ pretty_code_checks(
      .x$full_name,
      .x$email,
      .x$check_setup_output
    )) %>%
    stringr::str_c(collapse = "\n\n")
}

check_project_setup <- function(data) {
  data %>%
    dplyr::group_split(full_name) %>%
    purrr::map_chr(~ pretty_code_checks(
      .x$full_name,
      .x$email,
      .x$check_project_setup_output
    )) %>%
    stringr::str_c(collapse = "\n\n\n")
}

check_problem_description <- function(data) {
  data %>%
    dplyr::group_split(full_name) %>%
    purrr::map_chr(~ pretty_text_checks(
      .x$full_name,
      .x$email,
      paste0(.x$describe_problems, "\n\n", .x$when_available_for_help)
    )) %>%
    stringr::str_c(collapse = "\n\n")
}
