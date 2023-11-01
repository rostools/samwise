

# Intermediate feedback ------------------------------------------------

inter_feedback <- fetch_feedback_inter()

inter_feedback %>%
  extract_feedback_quantitative() %>%
  save_as_csv("data/inter/feedback-quantitative.csv")

inter_feedback %>%
  extract_feedback_sessions() %>%
  save_as_csv("data/inter/feedback-sessions.csv")

inter_feedback %>%
  extract_feedback_overall() %>%
  save_as_csv("data/inter/feedback-overall.csv")

# Advanced feedback ---------------------------------------------------

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

