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

run_if_workshop_month_away <- function() {
  upcoming <- get_upcoming_workshop()

  if (is.na(upcoming)) {
    return(FALSE)
  }

  closest_date <- upcoming |>
    get_upcoming_workshop_dates() |>
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
    name = upcoming_preworkshop_survey,
    command = if (!is.na(get_upcoming_workshop())) {
      survey <- get_upcoming_workshop() |>
        get_preworkshop_survey()
      if (nrow(survey) > 0) {
        survey <- survey |>
          dplyr::filter(workshop_date == max(workshop_date))
      }
      survey
    } else {
      NA
    },
    force = run_if_workshop_month_away()
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
    name = workshop_ids,
    command = list_workshop_ids()
  ),

  # Pre-workshop survey -------------------------------------------------------
  tar_target(
    name = preworkshop_surveys,
    command = get_preworkshop_survey(workshop_ids),
    pattern = map(workshop_ids),
    iteration = "list"
  ),
  tar_target(
    name = participant_overview,
    command = extract_participant_overview(preworkshop_surveys),
    pattern = map(preworkshop_surveys),
    iteration = "list"
  ),
  tar_target(
    name = saved_participant_overview,
    command = {
      if (!is.null(participant_overview)) {
        participant_overview |>
          dplyr::mutate(type = "overview") |>
          save_responses_to_csv(c("workshop_id", "workshop_date", "type"))
      }
    },
    format = "file",
    pattern = map(participant_overview)
  ),

  # Feedback ----------------------------------------------------------------
  tar_target(
    name = preworkshop_feedback,
    command = if (!is.null(preworkshop_surveys)) {
      extract_preworkshop_feedback(preworkshop_surveys)
    },
    pattern = map(preworkshop_surveys),
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
        columns = c("workshop_id", "workshop_date", "type")
      ),
      list(
        data = feedback_survey_quantitative |>
          dplyr::mutate(type = "feedback-quantitative"),
        columns = c("workshop_id", "workshop_date", "type")
      ),
      list(
        data = feedback_survey_sessions |>
          dplyr::mutate(type = "feedback-sessions"),
        columns = c("workshop_id", "workshop_date", "type", "date")
      )
    )
  ),
  tar_target(
    name = saved_feedback_sessions_paths,
    command = combined_feedback |>
      purrr::map(
        \(feedback) {
          if (nrow(feedback$data) > 0) {
            save_responses_to_csv(feedback$data, feedback$columns)
          } else {
            NA
          }
        }
      ) |>
      unlist() |>
      na.omit(),
    format = "file"
  ),
  tar_target(
    name = saved_preworkshop_feedback_paths,
    command = {
      if (!is.null(preworkshop_feedback)) {
        preworkshop_feedback |>
          dplyr::mutate(type = "feedback-preworkshop") |>
          save_responses_to_csv(c("workshop_id", "workshop_date", "type"))
      }
    },
    format = "file",
    pattern = map(preworkshop_feedback),
  )
)
