
#' Post a Course Planning issue on the course repo.
#'
#' @param repo Either "r-cubed" or "r-cubed-intermediate"
#' @param course_date The date of the first day of the course, as YYYY-MM-DD
#' @param host Whether it is GitHub or GitLab
#'
#' @return NULL. Used only for the side effects of posting an issue.
#' @export
#'
admin_create_planning_issue <- function(repo, course_date, host = c("gitlab", "github")) {
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

  host <- rlang::arg_match(host)

  if (host == "gitlab") {
    gitlabr::set_gitlab_connection(
      gitlab_url = "https://gitlab.com/",
      private_token = askpass::askpass("Provide your GitLab PAT.")
    )

    project_id <- switch(repo,
      "r-cubed-intermediate" = "20120886",
      "r-cubed" = "15345313"
    )

    gitlabr::gl_new_issue(
      project = project_id,
      title = paste0("Course planning and details - ", course_date),
      description = paste0(issue_description, collapse = "\n"),
      labels = "Admin"
    )
  } else if (host == "github") {
    githubr::gh_new_issue(
      repo = paste0("rostools/", repo),
      title = paste0("Course planning and details - ", course_date),
      body = paste0(issue_description, collapse = "\n")
    )
  }

  return(invisible(NULL))
}
