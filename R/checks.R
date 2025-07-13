# Check who hasn't finished the survey ------------------------------------

check_who_not_finish_survey <- function(data, participant_list) {
  data |>
    dplyr::select(
      full_name = what_is_your_full_name,
      name_from_survey = what_is_your_full_name,
      email_from_survey = email_address,
      email = email_address
    ) |>
    dplyr::full_join(
      participant_list |>
        dplyr::select(email) |>
        dplyr::mutate(email_from_list = email),
      by = "email"
    ) |>
    dplyr::select(
      full_name,
      tidyselect::contains("name"),
      tidyselect::contains("email")
    )
}

copy_emails_for_reminder <- function(data) {
  data |>
    dplyr::filter(is.na(name_from_survey)) |>
    dplyr::pull(email_from_list) |>
    clipr::write_clip()
}

copy_reminder_email_text <- function() {
  fs::path_package("samwise", "templates", "email-reminder.txt") |>
    readr::read_lines() |>
    clipr::write_clip()
}

# Check who has problems --------------------------------------------------

check_precourse_problems <- function(data) {
  data |>
    dplyr::filter(
      did_you_encounter_any_problems_during_the_pre_course_tasks == "Yes"
    ) |>
    dplyr::select(tidyselect::contains(c(
      "full_name",
      "email",
      "check_setup",
      "check_project_setup",
      "describe_the_problems",
      "would_you_be_available"
    )))
}

copy_names_with_problems <- function(full_name) {
  full_name |>
    stringr::str_c("- ", ., collapse = "\n") |>
    append("People with problems:", after = 0) |>
    stringr::str_c(collapse = " \n") |>
    clipr::write_clip()
}

# Setup checks ------------------------------------------------------------

pretty_code_checks <- function(name, email, check) {
  glue::glue(
    "\n#### {unique(name)}  ({unique(email)})\n\n```\n{check}\n```\n\n"
  )
}

pretty_text_checks <- function(name, email, check) {
  glue::glue("\n#### {unique(name)} ({unique(email)})\n\n{check}\n\n")
}

check_setup <- function(data) {
  data |>
    dplyr::select(
      full_name = what_is_your_full_name,
      email = email_address,
      check_setup = copy_and_paste_the_results_of_your_r_3_check_setup_into_the_text_box_below
    ) |>
    dplyr::group_split(full_name) |>
    purrr::map_chr(
      ~ pretty_code_checks(
        .x$full_name,
        .x$email,
        .x$check_setup
      )
    ) |>
    stringr::str_c(collapse = "\n\n")
}

check_project_setup <- function(data) {
  data |>
    dplyr::rename_with(
      \(cols) {
        stringr::str_replace(
          cols,
          "copy_and_paste_the_results_of_your_r_3_check_project_setup.*",
          "check_project_setup"
        )
      }
    ) |>
    dplyr::select(
      full_name = what_is_your_full_name,
      email = email_address,
      check_project_setup
    ) |>
    dplyr::group_split(full_name) |>
    purrr::map_chr(
      ~ pretty_code_checks(
        .x$full_name,
        .x$email,
        .x$check_project_setup
      )
    ) |>
    stringr::str_c(collapse = "\n\n\n")
}

check_problem_description <- function(data) {
  data |>
    dplyr::select(
      full_name = what_is_your_full_name,
      email = email_address,
      describe_problems = please_describe_the_problems_you_ve_had,
      when_available_for_help = which_dates_would_you_be_available_for_a_video_call_to_help_with_the_problems
    ) |>
    dplyr::group_split(full_name) |>
    purrr::map_chr(
      ~ pretty_text_checks(
        .x$full_name,
        .x$email,
        paste0(.x$describe_problems, "\n\n", .x$when_available_for_help)
      )
    ) |>
    stringr::str_c(collapse = "\n\n")
}
