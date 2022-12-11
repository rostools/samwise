
devtools::load_all()

# For creating random ids
set.seed(125643)

# Advanced feedback -------------------------------------------------------

advanced_feedback <- fetch_feedback_advanced()

advanced_feedback %>%
  extract_feedback_quantitative() %>%
  save_as_csv("data/advanced/feedback.csv")

advanced_feedback %>%
  extract_feedback_sessions() %>%
  save_as_csv("data/advanced/sessions.csv")

advanced_feedback %>%
  extract_feedback_overall() %>%
  save_as_csv("data/advanced/overall.csv")

