
devtools::load_all()

# Introductory pre-course survey ----------------------------------------------

intro_precourse <- fetch_precourse_intro()

intro_precourse %>%
  extract_precourse_overview(intro_survey_column_renaming) %>%
  save_as_csv("data/intro/participant-overview.csv")

intro_precourse %>%
  extract_precourse_feedback(intro_survey_column_renaming) %>%
  save_as_csv("data/intro/precourse-feedback.csv")

# Advanced pre-course survey ----------------------------------------------

advanced_precourse <- fetch_precourse_advanced()

advanced_precourse %>%
  extract_precourse_overview(advanced_survey_column_renaming) %>%
  save_as_csv("data/advanced/participant-overview.csv")

advanced_precourse %>%
  extract_precourse_feedback(advanced_survey_column_renaming) %>%
  save_as_csv("data/advanced/precourse-feedback.csv")
