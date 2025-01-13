# These are for admin tasks that really only need to be done once or twice

@_default:
  just --list --unsorted

# Run build recipes and install the package
build: install-package-dependencies style document install-package

# Install package dependencies
install-package-dependencies:
  #!/usr/bin/Rscript
  pak::pak(ask = FALSE)

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
run-targets:
  #!/usr/bin/Rscript
  targets::tar_make()

# (INCOMPLETE) Create a draft email to remind participants of precourse tasks
draft-reminder-email: run-targets
  #!/usr/bin/Rscript
  participant_emails <- get_participant_emails()
  r3admin::create_draft_reminder_email()

# Create a GitHub issue for planning the next upcoming course
create-upcoming-planning-issue: build
  #!/usr/bin/Rscript
  r3admin::admin_create_planning_issue(
    id = r3admin::get_upcoming_course()
  )

# Create the PDF and HTML files used for putting people into the groups
create-group-names:
  #!/usr/bin/Rscript
  devtools::load_all()
  precourse <- targets::tar_read(upcoming_precourse_survey)
  # TODO: Move team name creation into targets? Output a team-name file?
  number_groups <- ceiling(nrow(precourse) / 4)
  even_number_people <- round(nrow(precourse) + 0.5)
  group_names <- create_group_names(number_groups)
  readr::write_lines(group_names, here::here("_ignore/group-names.txt"))
  group_names_to_one_pdf(group_names)
  Sys.sleep(1.5)
  group_names_as_strips_html(group_names, number_participants = even_number_people)

