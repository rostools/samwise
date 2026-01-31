# These are for admin tasks that really only need to be done once or twice

@_default:
  just --list --unsorted

# Run build recipes and install the package
build: install-deps style check-spelling document install

# Install package dependencies
install-deps:
  #!/usr/bin/Rscript
  pak::pak(ask = FALSE, upgrade = TRUE)

# Install the package and its dependencies
install:
  #!/usr/bin/Rscript
  devtools::install()

# Re-build Roxygen docs
document:
  #!/usr/bin/Rscript
  devtools::document()

# Run style formatter
style:
  air format .

# Run linter
lint:
  jarl check .

# Run spell checker
check-spelling:
  uvx typos

# Run the targets pipeline
run-targets:
  #!/usr/bin/Rscript
  targets::tar_make()

# (INCOMPLETE) Create a draft email to remind participants of preworkshop tasks
draft-reminder-email: run-targets
  #!/usr/bin/Rscript
  participant_emails <- get_participant_emails()
  samwise::create_draft_reminder_email()

# Create a GitHub issue for planning the next upcoming workshop
create-upcoming-planning-issue: build
  #!/usr/bin/Rscript
  samwise::admin_create_planning_issue(
    id = samwise::get_upcoming_workshop()
  )

# Create the PDF and HTML files used for putting people into the groups
create-group-names:
  #!/usr/bin/Rscript
  devtools::load_all()
  preworkshop <- targets::tar_read(upcoming_preworkshop_survey)
  # TODO: Move team name creation into targets? Output a team-name file?
  number_groups <- ceiling(nrow(preworkshop) / 4)
  even_number_people <- round(nrow(preworkshop) + 0.5)
  group_names <- create_group_names(number_groups)
  readr::write_lines(group_names, here::here("_ignore/group-names.txt"))
  group_names_to_one_pdf(group_names)
  Sys.sleep(1.5)
  group_names_as_strips_html(group_names, number_participants = even_number_people)

