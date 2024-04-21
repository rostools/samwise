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
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # Upcoming (soonest) ------------------------------------------------------
  # tar_target(
  #   name = precourse_survey,
  #   command = get_precourse_survey(get_upcoming_course()),
  # ),
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
    pattern = map(course_ids)
  ),
  tar_target(
    name = participant_overview,
    command = extract_participant_overview(precourse_surveys),
    pattern = map(precourse_surveys)
  ),

  # Feedback ----------------------------------------------------------------
  tar_target(
    name = precourse_feedback,
    command = extract_precourse_feedback(precourse_surveys),
    pattern = map(precourse_surveys)
  ),
  tar_target(
    name = course_ids_feedback,
    command = c(list_course_ids(), "general")
  ),
  tar_target(
    name = feedback_survey,
    command = get_feedback_survey(course_ids_feedback),
    pattern = map(course_ids_feedback)
  ),
  tar_target(
    name = feedback_survey_overall,
    command = extract_feedback_overall(feedback_survey),
    pattern = map(feedback_survey)
  ),
  tar_target(
    name = feedback_survey_quantitative,
    command = extract_feedback_quantitative(feedback_survey),
    pattern = map(feedback_survey)
  ),
  tar_target(
    name = feedback_survey_sessions,
    command = extract_feedback_sessions(feedback_survey),
    pattern = map(feedback_survey)
  )
)
