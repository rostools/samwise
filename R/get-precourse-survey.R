
# Import pre-survey data --------------------------------------------------

#' Get the (slightly cleaned) pre-course survey from Google Sheets.
#'
#' @param id The ID of the course, found by running [list_course_ids()].
#'
#' @return A [tibble::tibble()].
#' @export
#'
#' @examples
#' get_precourse_survey("intro")
get_precourse_survey <- function(id) {
  id <- rlang::arg_match(id, list_course_ids())

  # Get the Google Sheet ID from the environment variable via `Sys.getenv()`
  survey_id <- switch(
    id,
    intro = "INTRO_PRE_SURVEY_ID",
    inter = "INTERMEDIATE_PRE_SURVEY_ID",
    adv = "ADVANCED_PRE_SURVEY_ID"
  )
  survey_id <- Sys.getenv(survey_id)
  if (survey_id == "") {
    cli::cli_abort("{.fn Sys.genenv} can't find the Google Sheet ID, do you have an {.val .Renviron} set up with the ID?")
  }

  survey_id |>
    get_precourse_survey_google_sheet() |>
    tidy_precourse(get_course_dates(id)) |>
    dplyr::mutate(course_id = id, .before = tidyselect::everything())
}

# Get survey from Google --------------------------------------------------

get_precourse_survey_google_sheet <- function(survey_id) {
  googledrive::drive_get(id = survey_id) |>
    googlesheets4::read_sheet()
}

# Tidy up the survey data -------------------------------------------------

tidy_precourse <- function(data, metadata_dates) {
  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.list),
      ~ purrr::map_chr(.x, as.character)
    )) |>
    dplyr::rename_with(snakecase::to_snake_case) |>
    dplyr::rename(github_username = what_is_your_git_hub_user_name,
                  email = email_address) |>
    dplyr::mutate(
      course_version = assign_course_version_by_date(timestamp, metadata_dates)
    ) |>
    dplyr::mutate(dplyr::across(
      tidyselect::contains("_perceive_your_skill_"),
      tidy_cols_skills
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("^github_username$"),
      ~ stringr::str_remove(.x, pattern = "\\@")
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
      course_version,
      tidyselect::starts_with("perceived"),
      tidyselect::starts_with("uses"),
      gender_identity,
      research_position,
      city_work_in,
      previously_used_stat_programs,
      accept_conduct
    ) |>
    tidyr::pivot_longer(
      -course_version,
      names_to = "questions",
      values_to = "responses"
    ) |>
    dplyr::count(course_version, questions, responses, name = "count") |>
    dplyr::arrange(course_version, questions, responses, count) |>
    join_original_column_names(column_renaming_df) %>%
    dplyr::mutate(
      questions = questions %>%
        stringr::str_replace(
          "^How .* perceive .*\\.\\.\\. \\[(.*)\\]$",
          "Perceived skill/knowledge in \\1"
        ) %>%
        stringr::str_remove_all("\\[|\\]")
    ) |>
    dplyr::relocate(course_version, questions, responses, count)
}

#' @describeIn extract_precourse Extract and tidy up the pre-course feedback
#'  data.
#' @export
extract_precourse_feedback <- function(data, column_renaming_df) {
  data |>
    sanitize_precourse() |>
    dplyr::select(
      course_version,
      tidyselect::contains("feedback"),
      describe_problems,
      tidyselect::contains("course_expectations"),
      tidyselect::contains("why_attend_course")
    ) |>
    tidyr::pivot_longer(
      -course_version,
      names_to = "questions",
      values_to = "responses"
    ) |>
    dplyr::arrange(course_version, questions, responses) |>
    join_original_column_names(column_renaming_df) |>
    dplyr::relocate(course_version, questions, responses)
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
    dplyr::left_join(column_renaming_df, by = c("questions" = "new_column_names")) |>
    tidyr::drop_na() |>
    dplyr::select(-questions, questions = original_column_names) |>
    dplyr::relocate(questions)
}
