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

# Set seed for random id generation
set.seed(125643)

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
    command = extract_participant_overview(precourse_surveys, course_ids),
    pattern = map(course_ids)
  ),

  # Introduction course -----------------------------------------------------
  tar_force(
    name = intro_feedback,
    command = get_feedback_survey(course_ids),
    pattern = map(course_ids)
    force = TRUE
  ),
  tar_target(
    name = intro_feedback_quantitative,
    command = intro_feedback %>%
      extract_feedback_quantitative() %>%
      save_as_csv("data/intro/feedback-quantitative.csv"),
    format = "file"
  ),
  tar_target(
    name = intro_feedback_sessions,
    command = intro_feedback %>%
      extract_feedback_sessions() %>%
      save_as_csv("data/intro/feedback-sessions.csv"),
    format = "file"
  ),
  tar_target(
    name = intro_feedback_overall,
    command = intro_feedback %>%
      extract_feedback_overall() %>%
      save_as_csv("data/intro/feedback-overall.csv"),
    format = "file"
  ),

  # Intermediate course -----------------------------------------------------
  tar_force(
    name = inter_feedback,
    command = fetch_feedback_inter(),
    force = TRUE
  ),
  tar_target(
    name = inter_feedback_quantitative,
    command = inter_feedback %>%
      extract_feedback_quantitative() %>%
      save_as_csv("data/inter/feedback-quantitative.csv"),
    format = "file"
  ),
  tar_target(
    name = inter_feedback_sessions,
    command = inter_feedback %>%
      extract_feedback_sessions() %>%
      save_as_csv("data/inter/feedback-sessions.csv"),
    format = "file"
  ),
  tar_target(
    name = inter_feedback_overall,
    command = inter_feedback %>%
      extract_feedback_overall() %>%
      save_as_csv("data/inter/feedback-overall.csv"),
    format = "file"
  ),

  # Advanced course -----------------------------------------------------
  tar_force(
    name = advanced_feedback,
    command = fetch_feedback_advanced(),
    force = TRUE
  ),
  tar_target(
    name = advanced_feedback_quantitative,
    command = advanced_feedback %>%
      extract_feedback_quantitative() %>%
      save_as_csv("data/advanced/feedback-quantitative.csv"),
    format = "file"
  ),
  tar_target(
    name = advanced_feedback_sessions,
    command = advanced_feedback %>%
      extract_feedback_sessions() %>%
      save_as_csv("data/advanced/feedback-sessions.csv"),
    format = "file"
  ),
  tar_target(
    name = advanced_feedback_overall,
    command = advanced_feedback %>%
      extract_feedback_overall() %>%
      save_as_csv("data/advanced/feedback-overall.csv"),
    format = "file"
  )
)
