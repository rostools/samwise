# Created by use_targets().

# Package loading and config setup ----------------------------------------

# Load packages required to define the pipeline:
library(targets)
library(magrittr)
library(tarchetypes)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("lubridate")
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Force re-running everything
# targets::tar_destroy()

run_if_course_month_away <- function() {
  upcoming <- get_upcoming_course()

  if (is.na(upcoming)) {
    return(FALSE)
  }

  closest_date <- upcoming |>
    get_upcoming_course_dates() |>
    lubridate::ymd()

  dplyr::between(
    lubridate::today(),
    closest_date - months(1),
    closest_date
  )
}

# Replace the target list below with your own:
list(
  # Upcoming (soonest) ------------------------------------------------------
  tar_force(
    name = upcoming_precourse_survey,
    command = if (!is.na(get_upcoming_course())) {
      survey <- get_upcoming_course() |>
        get_precourse_survey()
      if (nrow(survey) > 0) {
        survey <- survey |>
          dplyr::filter(course_date == max(course_date))
      }
      survey
    } else {
      NA
    },
    force = run_if_course_month_away()
  ),
  # tar_target(
  #   name = participants_not_complete_survey,
  #   command =
  # ),
  # tar_target(
  #   name = participants_with_problems,
  #   command =
  # ),
  # tar_target(
  #   name = check_setups,
  #   command =
  # ),
  # tar_target(
  #   name = create_team_pdfs,
  #   command = ,
  #   format = "file"
  # ),
  tar_target(
    name = course_ids,
    command = list_course_ids()
  ),

  # Pre-course survey -------------------------------------------------------
  tar_target(
    name = precourse_surveys,
    command = get_precourse_survey(course_ids),
    pattern = map(course_ids),
    iteration = "list"
  ),
  tar_target(
    name = participant_overview,
    command = extract_participant_overview(precourse_surveys),
    pattern = map(precourse_surveys),
    iteration = "list"
  ),
  tar_target(
    name = saved_participant_overview,
    command = {
      if (!is.null(participant_overview)) {
        participant_overview |>
          dplyr::mutate(type = "overview") |>
          save_responses_to_csv(c("course_id", "course_date", "type"))
      }
    },
    format = "file",
    pattern = map(participant_overview)
  ),

  # Feedback ----------------------------------------------------------------
  tar_target(
    name = precourse_feedback,
    command = if (!is.null(precourse_surveys)) {
      extract_precourse_feedback(precourse_surveys)
    },
    pattern = map(precourse_surveys),
    iteration = "list"
  ),
  tar_target(
    name = feedback_survey,
    command = get_feedback_survey()
  ),
  tar_target(
    name = feedback_survey_overall,
    command = extract_feedback_overall(feedback_survey)
  ),
  tar_target(
    name = feedback_survey_quantitative,
    command = extract_feedback_quantitative(feedback_survey)
  ),
  tar_target(
    name = feedback_survey_sessions,
    command = extract_feedback_sessions(feedback_survey)
  ),

  # Save to file -----------------------------------------------------------
  tar_target(
    name = combined_feedback,
    command = list(
      list(
        data = feedback_survey_overall |>
          dplyr::mutate(type = "feedback-overall"),
        columns = c("course_id", "course_date", "type")
      ),
      list(
        data = feedback_survey_quantitative |>
          dplyr::mutate(type = "feedback-quantitative"),
        columns = c("course_id", "course_date", "type")
      ),
      list(
        data = feedback_survey_sessions |>
          dplyr::mutate(type = "feedback-sessions"),
        columns = c("course_id", "course_date", "type", "date")
      )
    )
  ),
  tar_target(
    name = saved_feedback_sessions_paths,
    command = combined_feedback |>
      purrr::map(
        \(feedback) save_responses_to_csv(feedback$data, feedback$columns)
      ) |>
      unlist(),
    format = "file"
  ),
  tar_target(
    name = saved_preworkshop_feedback_paths,
    command = {
      if (!is.null(precourse_feedback)) {
        precourse_feedback |>
          dplyr::mutate(type = "feedback-precourse") |>
          save_responses_to_csv(c("course_id", "course_date", "type"))
      }
    },
    format = "file",
    pattern = map(precourse_feedback),
  )
)
