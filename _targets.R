# Created by use_targets().

# Package loading and config setup ----------------------------------------

# Load packages required to define the pipeline:
library(targets)
library(magrittr)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble") # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Set seed for random id generation
set.seed(125643)

# Replace the target list below with your own:
list(
  tar_target(
    name = data,
    command = tibble(x = rnorm(100), y = rnorm(100))
  ),

  # Introduction course -----------------------------------------------------
  tar_target(
    name = intro_feedback,
    command = fetch_feedback_intro()
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
  )
)
