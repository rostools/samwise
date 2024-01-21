
devtools::load_all()

# Introductory pre-course survey ----------------------------------------------

intro_precourse <- get_precourse_survey("intro")

intro_precourse %>%
  extract_precourse_overview(intro_survey_column_renaming) %>%
  save_as_csv("data/intro/participant-overview.csv")

intro_precourse %>%
  extract_precourse_feedback(intro_survey_column_renaming) %>%
  save_as_csv("data/intro/feedback-precourse.csv")

# Intermediate pre-course survey ----------------------------------------------

inter_precourse <- get_precourse_survey("inter")

inter_precourse %>%
  extract_precourse_overview(intermediate_survey_column_renaming) %>%
  save_as_csv("data/inter/participant-overview.csv")

inter_precourse %>%
  extract_precourse_feedback(intermediate_survey_column_renaming) %>%
  save_as_csv("data/inter/feedback-precourse.csv")

# Advanced pre-course survey --------------------------------------------------

advanced_precourse <- get_precourse_survey("adv")

advanced_precourse %>%
  extract_precourse_overview(advanced_survey_column_renaming) %>%
  save_as_csv("data/advanced/participant-overview.csv")

advanced_precourse %>%
  extract_precourse_feedback(advanced_survey_column_renaming) %>%
  save_as_csv("data/advanced/feedback-precourse.csv")
