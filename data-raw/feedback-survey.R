
devtools::load_all()

# For creating random ids
set.seed(125643)

# Intro feedback -------------------------------------------------------

intro_feedback <- fetch_feedback_intro()

intro_feedback %>%
  extract_feedback_quantitative() %>%
  save_as_csv("data/intro/feedback-quantitative.csv")

intro_feedback %>%
  extract_feedback_sessions() %>%
  save_as_csv("data/intro/feedback-sessions.csv")

intro_feedback %>%
  extract_feedback_overall() %>%
  save_as_csv("data/intro/feedback-overall.csv")

# Advanced feedback -------------------------------------------------------

advanced_feedback <- fetch_feedback_advanced()

advanced_feedback %>%
  extract_feedback_quantitative() %>%
  save_as_csv("data/advanced/feedback-quantitative.csv")

advanced_feedback %>%
  extract_feedback_sessions() %>%
  save_as_csv("data/advanced/feedback-sessions.csv")

advanced_feedback %>%
  extract_feedback_overall() %>%
  save_as_csv("data/advanced/feedback-overall.csv")

