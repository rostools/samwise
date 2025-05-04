#' Post a Course Planning issue on the course repo.
#'
#' @inheritParams get_course_metadata_field
#'
#' @return A GitHub API response. Used for the side effects of posting an issue.
#' @export
#'
admin_create_planning_issue <- function(id) {
  id <- rlang::arg_match(id, list_course_ids())
  repo <- get_course_repo(id)
  course_date <- get_upcoming_course_dates(id)

  if (length(course_date) == 0) {
    cli::cli_abort(
      "There are no upcoming dates for this course. Are you sure course {.val id} is the correct course and the metadata has been updated?"
    )
  }

  stamp_format <- lubridate::stamp_date("Mar. 1, 2021", quiet = TRUE)
  template_path <- fs::path_package("r3admin", "templates", "planning-issue.md")
  issue_description <- whisker::whisker.render(
    # Skip the yaml metadata
    readr::read_lines(template_path, skip = 7),
    data = list(
      repo_name = repo,
      course_date = course_date,
      instructors_precourse_meeting = stamp_format(
        as.Date(course_date) - lubridate::days(14)
      ),
      tasks_start_date = stamp_format(as.Date(course_date) - months(1)),
      tasks_remind_date = stamp_format(
        as.Date(course_date) - lubridate::days(8)
      ),
      tasks_check_end_date = stamp_format(
        as.Date(course_date) - lubridate::days(5)
      ),
      tasks_prep_end_date = stamp_format(
        as.Date(course_date) - lubridate::days(3)
      ),
      session_schedule_table = planning_issue_sessions_table(id) |>
        stringr::str_c(collapse = "\n")
    )
  )

  gh_api_results <- ghclass::issue_create(
    repo = paste0("rostools/", repo),
    title = paste0("Course planning and details - ", course_date),
    body = paste0(issue_description, collapse = "\n")
  )

  return(invisible(gh_api_results))
}

planning_issue_sessions_table <- function(id) {
  get_schedule_sessions(id) |>
    dplyr::rename_with(stringr::str_to_sentence) |>
    dplyr::mutate(Instructor = "") |>
    knitr::kable() |>
    as.character()
}
