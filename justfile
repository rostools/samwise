# These are for admin tasks that really only need to be done once or twice

@_default:
  just --list --unsorted

# Run build recipes and install the package
build: style document install-package

# Install the package and its dependencies
install-package:
  #!/usr/bin/Rscript
  devtools::install()

# Re-build Roxygen docs
document:
  #!/usr/bin/Rscript
  devtools::document()

# Run style formatter
style:
  #!/usr/bin/Rscript
  styler::style_pkg()

# Run the targets pipeline
run-targets: build
  #!/usr/bin/Rscript
  targets::tar_make()

# Create the draft email to participants
draft-reminder-email: run-targets
  #!/usr/bin/Rscript
  participant_emails <- get_participant_emails()
  r3admin::create_draft_reminder_email()

create-org-in-gh:
  xdg-open https://github.com/account/organizations/new?plan=free

# Create the planning issue for the upcoming course
create-upcoming-planning-issue: build
  #!/usr/bin/Rscript
  r3admin::admin_create_planning_issue(
    id = r3admin::get_upcoming_course()
  )

create-group-names: run-targets
  #!/usr/bin/Rscript
  devtools::load_all()
  precourse <- targets::tar_read(upcoming_precourse_survey)
  number_groups <- ceiling(nrow(precourse) / 4)
  group_names <- create_group_names(number_groups)
  group_names_to_one_pdf(group_names)
  Sys.sleep(1)
  group_names_as_strips_html(group_names, number_participants = nrow(precourse))
