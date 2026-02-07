# Import pre-survey data --------------------------------------------------

#' Get the (slightly cleaned) pre-workshop survey from Google Sheets.
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' get_preworkshop_survey()
#' }
get_preworkshop_survey <- function() {
  get_preworkshop_survey_google_sheet() |>
    tidy_preworkshop()
}

# Get survey from Google --------------------------------------------------

get_preworkshop_survey_google_sheet <- function(n_max = Inf) {
  # Get the Google Sheet ID from the environment variable via `Sys.getenv()`
  survey_id <- Sys.getenv("PREWORKSHOP_SURVEY_ID")

  if (survey_id == "") {
    cli::cli_abort(
      "{.fn Sys.getenv} can't find the Google Sheet ID, do you have an {.val .Renviron} set up with the ID?"
    )
  }

  googledrive::drive_get(id = survey_id) |>
    googlesheets4::read_sheet(n_max = n_max)
}

# Tidy up the survey data -------------------------------------------------

tidy_preworkshop <- function(data) {
  workshop_names <- tibble::tibble(
    workshop_id = stringr::str_subset(
      list_workshop_ids(),
      "general",
      negate = TRUE
    ),
    workshop_name = purrr::map_chr(
      workshop_id,
      ~ get_workshop_metadata_field(.x, "name")
    )
  )

  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.list),
      ~ purrr::map_chr(.x, as.character)
    )) |>
    dplyr::rename(workshop_name = "Which workshop is this for?") |>
    dplyr::rename_with(snakecase::to_snake_case) |>
    dplyr::left_join(workshop_names, by = "workshop_name") |>
    dplyr::mutate(
      workshop_date = purrr::map2_chr(
        timestamp,
        workshop_id,
        ~ assign_workshop_date_by_date(
          .x,
          lubridate::as_date(get_workshop_dates(.y))
        )
      )
    ) |>
    dplyr::mutate(dplyr::across(
      tidyselect::contains("_perceive_your_skill_"),
      tidy_cols_skills
    ))
}

skills_df <- tibble::tribble(
  ~number , ~text                   ,
  "1"     , "Beginner"              ,
  "2"     , "Beginner-Intermediate" ,
  "3"     , "Intermediate"          ,
  "4"     , "Intermediate-Advanced" ,
  "5"     , "Advanced"
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
