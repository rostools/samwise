
#' Post a Course Planning issue on the course repo.
#'
#' @param repo Either "r-cubed" or "r-cubed-intermediate"
#' @param course_date The date of the first day of the course, as YYYY-MM-DD
#'
#' @return NULL. Used only for the side effects of posting an issue.
#' @export
#'
admin_create_planning_issue <- function(repo, course_date, org = "rostools") {
  stamp_format <- lubridate::stamp_date("Mar. 1, 2021", quiet = TRUE)
  template_path <- fs::path_package("r3admin", "templates", "planning-issue.md")
  issue_description <- whisker::whisker.render(
    # Skip the yaml metadata
    readr::read_lines(template_path, skip = 7),
    data = list(
      repo_name = repo,
      course_date = course_date,
      instructors_precourse_meeting = stamp_format(as.Date(course_date) - lubridate::days(14)),
      tasks_start_date = stamp_format(as.Date(course_date) - months(1)),
      tasks_remind_date = stamp_format(as.Date(course_date) - lubridate::days(8)),
      tasks_check_end_date = stamp_format(as.Date(course_date) - lubridate::days(5)),
      tasks_prep_end_date = stamp_format(as.Date(course_date) - lubridate::days(3))
    )
  )

  gh_api_results <- ghclass::issue_create(
    repo = paste0(org, "/", repo),
    title = paste0("Course planning and details - ", course_date),
    body = paste0(issue_description, collapse = "\n")
  )

  return(invisible(gh_api_results))
}
