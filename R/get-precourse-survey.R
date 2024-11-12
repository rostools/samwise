# Import pre-survey data --------------------------------------------------

#' Get the (slightly cleaned) pre-course survey from Google Sheets.
#'
#' @inheritParams get_course_metadata_field
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' get_precourse_survey("intro")
#' }
get_precourse_survey <- function(id) {
  id <- rlang::arg_match(id, list_course_ids())
  id |>
    get_precourse_survey_google_sheet() |>
    tidy_precourse(get_course_dates(id)) |>
    dplyr::mutate(course_id = id, .before = tidyselect::everything())
}

# Get survey from Google --------------------------------------------------

get_precourse_survey_google_sheet <- function(id, n_max = Inf) {
  id <- rlang::arg_match(id, list_course_ids())

  # Get the Google Sheet ID from the environment variable via `Sys.getenv()`
  survey_id <- switch(id,
    intro = "INTRO_PRE_SURVEY_ID",
    inter = "INTERMEDIATE_PRE_SURVEY_ID",
    adv = "ADVANCED_PRE_SURVEY_ID"
  )
  survey_id <- Sys.getenv(survey_id)
  if (survey_id == "") {
    cli::cli_abort("{.fn Sys.genenv} can't find the Google Sheet ID, do you have an {.val .Renviron} set up with the ID?")
  }

  googledrive::drive_get(id = survey_id) |>
    googlesheets4::read_sheet(n_max = n_max)
}

# Tidy up the survey data -------------------------------------------------

tidy_precourse <- function(data, metadata_dates) {
  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.list),
      ~ purrr::map_chr(.x, as.character)
    )) |>
    dplyr::rename_with(snakecase::to_snake_case) |>
    dplyr::mutate(
      course_date = assign_course_date_by_date(timestamp, metadata_dates)
    ) |>
    dplyr::mutate(dplyr::across(
      tidyselect::contains("_perceive_your_skill_"),
      tidy_cols_skills
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
