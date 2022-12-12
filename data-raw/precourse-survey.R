
devtools::load_all()

# Advanced feedback -------------------------------------------------------

# Advanced pre-course survey ----------------------------------------------

advanced_precourse <- fetch_precourse_advanced()

advanced_precourse %>%
  extract_precourse_overview(advanced_survey_column_renaming) %>%
  save_as_csv("data/advanced/participant-overview.csv")

advanced_precourse %>%
  extract_precourse_feedback(advanced_survey_column_renaming) %>%
  save_as_csv("data/advanced/precourse-feedback.csv")
