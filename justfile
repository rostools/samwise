# These are for admin tasks that really only need to be done once or twice

install-package:
  #!/usr/bin/Rscript
  devtools::install()

draft-reminder-email: install-package
  #!/usr/bin/Rscript
  participant_emails <- get_participant_emails()
  r3admin::create_draft_reminder_email()

create-org-in-gh:
  xdg-open https://github.com/account/organizations/new?plan=free

create-upcoming-planning-issue: install-package
  #!/usr/bin/Rscript
  r3admin::admin_create_planning_issue(
    repo = r3admin::get_upcoming_course() |>
      r3admin::get_course_repo(),
    course_date =
  )


