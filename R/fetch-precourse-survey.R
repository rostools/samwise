
# Import pre-survey data --------------------------------------------------

#' Fetch the pre-course survey responses.
#'
#' @name fetch_precourse
#' @rdname fetch_precourse
#' @param survey_id Google Forms ID for the survey.
#'
#' @return Data from survey, slightly tidied up.
#'
#' @examples
#' \dontrun{
#' fetch_precourse_intro()
#' fetch_precourse_advanced()
#' }
#'
NULL

#' @describeIn fetch_precourse Fetch the pre-course survey data for the **introduction** course.
#' @export
fetch_precourse_intro <- function(survey_id = Sys.getenv("INTRO_PRE_SURVEY_ID")) {
  survey_id |>
    fetch_precourse_sheet() |>
    rename_columns_sentence_to_snakecase(intro_survey_column_renaming) |>
    check_duplicate_timestamps() %>%
    tidy_precourse(metadata$dates$introduction)
}

#' @describeIn fetch_precourse Fetch the pre-course survey data for the **advanced** course.
#' @export
fetch_precourse_advanced <- function(survey_id = Sys.getenv("ADVANCED_PRE_SURVEY_ID")) {
  survey_id |>
    fetch_precourse_sheet() |>
    rename_columns_sentence_to_snakecase(advanced_survey_column_renaming) |>
    check_duplicate_timestamps() %>%
    tidy_precourse(metadata$dates$advanced)
}

fetch_precourse_sheet <- function(survey_id) {
  googledrive::drive_get(id = survey_id) |>
    googlesheets4::read_sheet()
}

# Tidy up the survey data -------------------------------------------------

rename_columns_sentence_to_snakecase <- function(data, column_renaming_df) {
  renaming_vector <- column_renaming_df[
    c("new_column_names", "original_column_names")
  ] |>
    tibble::deframe()

  data |>
    dplyr::select(renaming_vector)
}

tidy_precourse <- function(data, metadata_dates) {
  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.list),
      ~ purrr::map_chr(.x, as.character)
    )) |>
    dplyr::mutate(
      research_position = .data$research_position |>
        stringr::str_to_sentence() |>
        stringr::str_replace("-", " ") |>
        stringr::str_replace("Phd", "PhD") |>
        stringr::str_replace("^PhD$", "PhD student"),
      city_work_in = .data$city_work_in |>
        stringr::str_replace("København", "Copenhagen") |>
        stringr::str_replace(".*(Copenhagen|Odense).*", "\\1"),
      expectations_match_syllabus = .data$expectations_match_syllabus |>
        stringr::str_to_sentence() |>
        stringr::str_remove_all("\\.") |>
        stringr::str_replace(".*(Yes).*", "\\1"),
      course_version = assign_course_version_by_date(.data$timestamp, metadata_dates)
    ) |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("^perceived_skill_"),
      tidy_cols_skills
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("^github_username$"),
      stringr::str_remove,
      pattern = "\\@"
    ))
}

skills_df <- tibble::tribble(
  ~number, ~text,
  "1", "Beginner",
  "2", "Beginner-Intermediate",
  "3", "Intermediate",
  "4", "Intermediate-Advanced",
  "5", "Advanced"
)

tidy_cols_skills_to_character <- function(x) {
  x |>
    stringr::str_replace_all(
      rlang::set_names(skills_df$text, skills_df$number)
    )
}

tidy_cols_skills_relevel <- function(x) {
  all_levels <- c(
    "Beginner",
    "Beginner-Intermediate",
    "Intermediate",
    "Intermediate-Advanced",
    "Advanced"
  )
  current_levels <- unique(stats::na.omit(x))
  forcats::fct_relevel(x, dplyr::intersect(all_levels, current_levels))
}

tidy_cols_skills <- function(x) {
  x |>
    tidy_cols_skills_to_character() |>
    tidy_cols_skills_relevel()
}

# Extract overview and feedback data --------------------------------------

#' Extract overview data from pre-course survey.
#'
#' @name extract_precourse
#' @rdname extract_precourse
#'
#' @param data Pre-survey data.
#' @param column_renaming_df Column that contains the renaming data.
#'
#' @return A tibble.
#'
NULL

#' @describeIn extract_precourse Extract and tidy up the pre-course survey
#'   participant overview data.
#' @export
extract_precourse_overview <- function(data, column_renaming_df) {
  data |>
    sanitize_precourse() |>
    dplyr::select(
      .data$course_version,
      tidyselect::starts_with("perceived"),
      tidyselect::starts_with("uses"),
      .data$gender_identity,
      .data$research_position,
      .data$city_work_in,
      .data$previously_used_stat_programs,
      .data$accept_conduct
    ) |>
    tidyr::pivot_longer(
      -.data$course_version,
      names_to = "Questions",
      values_to = "Responses"
    ) |>
    dplyr::count(.data$Questions, .data$Responses, name = "Count") |>
    dplyr::arrange(.data$Questions, .data$Responses, Count) |>
    join_original_column_names(column_renaming_df) %>%
    dplyr::mutate(
      Questions = .data$Questions %>%
        stringr::str_replace(
          "^How .* perceive .*\\.\\.\\. \\[(.*)\\]$",
          "Perceived skill/knowledge in \\1"
        ) %>%
        stringr::str_remove_all("\\[|\\]")
    )
}

#' @describeIn extract_precourse Extract and tidy up the pre-course feedback
#'  data.
#' @export
extract_precourse_feedback <- function(data, column_renaming_df) {
  data |>
    sanitize_precourse() |>
    dplyr::select(
      .data$course_version,
      tidyselect::contains("feedback"),
      .data$describe_problems,
      tidyselect::contains("course_expectations"),
      tidyselect::contains("why_attend_course")
    ) |>
    tidyr::pivot_longer(
      -.data$course_version,
      names_to = "Questions",
      values_to = "Responses"
    ) |>
    dplyr::arrange(Questions, Responses) |>
    join_original_column_names(column_renaming_df)
}

sanitize_precourse <- function(data) {
  data |>
    dplyr::select(
      -tidyselect::contains("email"),
      -tidyselect::contains("name"),
      -tidyselect::contains("github_username"),
      -timestamp
    ) |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      ~ stringr::str_remove_all(.x, '\\n|\\"')
    )) |>
    dplyr::select(tidyselect::where(~ !all(is.na(.x))))
}

join_original_column_names <- function(data, column_renaming_df) {
  data |>
    dplyr::left_join(column_renaming_df, by = c("Questions" = "new_column_names")) |>
    tidyr::drop_na() |>
    dplyr::select(-.data$Questions, Questions = .data$original_column_names) |>
    dplyr::relocate(.data$Questions)
}

# Checks ------------------------------------------------------------------

# Renaming pre-course columns ---------------------------------------------

#' Fetch the column headers/titles and copy to the clipboard to tidy up.
#'
#' @param survey_id The ID for the Google Forms survey.
#'
#' @return Used for side effect of copying to clipboard.
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_survey_field_titles(Sys.getenv("INTRO_PRE_SURVEY_ID"))
#' fetch_survey_field_titles(Sys.getenv("INTERMEDIATE_PRE_SURVEY_ID"))
#' fetch_survey_field_titles(Sys.getenv("ADVANCED_PRE_SURVEY_ID"))
#' fetch_survey_field_titles(Sys.getenv("INTRO_FEEDBACK_SURVEY_ID"))
#' fetch_survey_field_titles(Sys.getenv("INTERMEDIATE_FEEDBACK_SURVEY_ID"))
#' fetch_survey_field_titles(Sys.getenv("ADVANCED_FEEDBACK_SURVEY_ID"))
#' }
fetch_survey_field_titles <- function(survey_id) {
  precourse <- googledrive::drive_get(id = survey_id) |>
    googlesheets4::read_sheet(n_max = 1)
  datapasta::vector_construct_vertical(names(precourse)) |>
    stringr::str_replace("^c\\(", "tibble::tribble(\n~original_column_names, ~new_column_names,\n") |>
    clipr::write_clip()
}

intro_survey_column_renaming <-
  tibble::tribble(
    ~original_column_names, ~new_column_names,
    "Timestamp", "timestamp",
    "Email Address", "email",
    "What is your full name?", "full_name",
    "What is your formal position?", "research_position",
    "What city do you work in or near?", "city_work_in",
    "Very briefly, what is your research topic(s)?", "research_topic",
    "How do you perceive your skill/knowledge of [using R?]", "perceived_skill_r",
    "What programs have you previously used for data analysis?", "previously_used_stat_programs",
    "What is your GitHub user name?", "github_username",
    "Copy and paste the results of your \"r3::check_setup()\" into the text box below.", "check_setup_output",
    "Why do you want to attend this course?", "why_attend_course",
    "Do your expectations match with what is described in the syllabus?", "expectations_match_syllabus",
    "Does our \"assumptions about who you are\" (in the syllabus) match with who you actually are? Why or why not?", "matched_assumptions",
    "Do you accept the conditions laid out in the Code of Conduct?", "accept_conduct",
    "In your opinion, what worked well?", "feedback_worked_well",
    "In your opinion, what could be improved?", "feedback_to_improve",
    "Did you encounter any problems during the pre-course tasks?", "encounter_problems",
    "Please describe the problems you've had.", "describe_problems",
    "Which dates would you be available for a video call to help with the problems?", "when_available_for_help",
    "How do you perceive your skill/knowledge of [data analysis in general?]", "perceived_skill_data_analysis",
    "How do you perceive your skill/knowledge of [programming in general?]", "perceived_skill_programming",
    "How do you perceive your skill/knowledge of [formal version control (e.g. Git)?]", "perceived_skill_git",
    "How often do you currently use: [dplyr]", "uses_dplyr",
    "How often do you currently use: [tidyr]", "uses_tidyr",
    "How often do you currently use: [pipe (%>%)]", "uses_pipe",
    "How often do you currently use: [GitHub/GitLab and Git]", "uses_git_github",
    "How often do you currently use: [R Markdown]", "uses_rmarkdown",
    "How often do you currently use: [ggplot2]", "uses_ggplot2",
    "What gender do you identify with?", "gender_identity",
    "What do you expect to learn and what would you like to be able to do with what you've learned?", "course_expectations"
  )

intermediate_survey_column_renaming <- tibble::tribble(
  ~original_column_names, ~new_column_names,
  "Timestamp", "timestamp",
  "Email Address", "email",
  "What is your full name?", "full_name",
  "What is your formal position?", "research_position",
  "What city do you work in or nearest to?", "city_work_in",
  "Very briefly, what is your research topic(s)?", "research_topic",
  "How do you perceive your skill/knowledge of... [using R?]", "perceived_skill_r",
  "How do you perceive your skill/knowledge of... [data analysis in general?]", "perceived_skill_data_analysis",
  "How do you perceive your skill/knowledge of... [programming in general?]", "perceived_skill_programming",
  "How do you perceive your skill/knowledge of... [managing data in general?]", "perceived_skill_manage_data",
  "How do you perceive your skill/knowledge of... [formal version control (e.g. Git)?]", "perceived_skill_git",
  "How do you perceive your skill/knowledge of... [writing in or using R Markdown?]", "perceived_skill_rmd",
  "What programs have you previously used for data analysis?", "previously_used_stat_programs",
  "Copy and paste the results of your \"r3::check_setup()\" into the text box below.", "check_setup_output",
  "Copy and paste the results of your \"r3::check_project_setup()\" into the text box below.", "check_project_setup_output",
  "What do you expect to learn from this course and what would you like to be able to do afterward with what you've learned?", "course_expectations",
  "Do your expectations match with what is described in the syllabus?", "expectations_match_syllabus",
  "Does our \"assumptions about who you are\" (in the syllabus) match with who you actually are? Why or why not?", "matched_assumptions",
  "Do you accept the conditions laid out in the Code of Conduct?", "accept_conduct",
  "In your opinion, what worked well?", "feedback_worked_well",
  "In your opinion, what could be improved?", "feedback_to_improve",
  "Did you encounter any problems during the pre-course tasks?", "encounter_problems",
  "Please describe the problems you've had.", "describe_problems",
  "Which dates would you be available for a video call to help with the problems?", "when_available_for_help",
  "What gender do you identify with?", "gender_identity",
  "How often do you currently use: [dplyr]", "uses_dplyr",
  "How often do you currently use: [tidyr]", "uses_tidyr",
  "How often do you currently use: [pipe (%>%)]", "uses_pipe",
  "How often do you currently use: [GitHub/GitLab and Git]", "uses_git_github",
  "How often do you currently use: [R Markdown]", "uses_rmarkdown",
  "How often do you currently use: [purrr]", "uses_purrr",
  "How often do you currently use: [tidyverse]", "uses_tidyverse",
  "How often do you currently use: [vroom/readr]", "uses_vroom"
)

advanced_survey_column_renaming <- tibble::tribble(
  ~original_column_names, ~new_column_names,
  "Timestamp", "timestamp",
  "Email Address", "email",
  "What is your full name?", "full_name",
  "What gender do you identify with?", "gender_identity",
  "What is your formal position?", "research_position",
  "What city do you work in or nearest to?", "city_work_in",
  "Very briefly, what is your research topic(s)?", "research_topic",
  "How do you perceive your skill/knowledge of... [using R?]", "perceived_skill_r",
  "How do you perceive your skill/knowledge of... [data analysis in general?]", "perceived_skill_data_analysis",
  "How do you perceive your skill/knowledge of... [programming in general?]", "perceived_skill_programming",
  "How do you perceive your skill/knowledge of... [writing reproducible code in general?]", "perceived_skill_repro_code",
  "How do you perceive your skill/knowledge of... [formal version control (e.g. Git)?]", "perceived_skill_git",
  "How do you perceive your skill/knowledge of... [collaborating through Git?]", "perceived_skill_git_collab",
  "How do you perceive your skill/knowledge of... [writing in or using R Markdown?]", "perceived_skill_rmd",
  "How often do you currently use: [tidyverse]", "uses_tidyverse",
  "How often do you currently use: [GitHub/GitLab and Git]", "uses_git_github",
  "How often do you currently use: [R Markdown/Quarto]", "uses_rmd_quarto",
  "How often do you currently use: [purrr]", "uses_purrr",
  "How often do you currently use: [renv]", "uses_renv",
  "How often do you currently use: [targets]", "uses_targets",
  "How often do you currently use: [styler]", "uses_styler",
  "How often do you currently use: [lintr]", "uses_lintr",
  "How often do you currently use: [tidymodels]", "uses_tidymodels",
  "What programs have you previously used for data analysis?", "previously_used_stat_programs",
  "Copy and paste the results of your \"r3::check_setup()\" into the text box below.", "check_setup_output",
  "Copy and paste the results of your \"r3::check_project_setup_advanced()\" into the text box below.", "check_project_setup_output",
  "What do you expect to learn from this course and what would you like to be able to do afterwards with what you've learned?", "course_expectations",
  "Do your expectations match with what is described in the syllabus?", "expectations_match_syllabus",
  "Does our \"Is this for you?\" (in the syllabus) match with who you actually are? Why or why not?", "matched_assumptions",
  "Do you accept the conditions laid out in the Code of Conduct?", "accept_conduct",
  "In your opinion, what worked well?", "feedback_worked_well",
  "In your opinion, what could be improved?", "feedback_to_improve",
  "Did you encounter any problems during the pre-course tasks?", "encounter_problems",
  "Please describe the problems you've had.", "describe_problems",
  "Which dates would you be available for a video call to help with the problems?", "when_available_for_help"
)
