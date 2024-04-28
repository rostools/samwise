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
    id = r3admin::get_upcoming_course()
  )

create-group-names: install-package
  #!/usr/bin/Rscript
  devtools::load_all()
  precourse <- targets::tar_read(upcoming_precourse_survey)
  number_groups <- ceiling(nrow(precourse) / 4)
  group_names <- create_group_names(number_groups)
  group_names_to_one_pdf(group_names)
  Sys.sleep(1)
  group_names_as_strips_html(group_names, number_participants = nrow(precourse))
