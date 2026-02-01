#' Create the necessary files for group names for a workshop.
#'
#' @param number_participants Number of participants in the workshop.
#'
#' @returns Invisibly returns TRUE. Side effect is to create a PDF file of each
#'    group name to be printed for each table, an HTML file with name strips to
#'    cut out for each participant for each day, as well as a text file with
#'    the group names for use in other functions.
#' @export
#'
create_group_files <- function(number_participants) {
  number_groups <- ceiling(number_participants / 4)
  even_number_people <- round(number_participants + 0.5)
  group_names <- create_group_names(number_groups)
  readr::write_lines(group_names, here::here("_ignore/group-names.txt"))
  cli::cli_alert_success(
    "Created group names file at {.path _ignore/group-names.txt}"
  )
  cli::cli_alert_info(
    "Creating the group names as PDF in {.path _ignore/group-names/}"
  )
  group_names_to_one_pdf(group_names)
  Sys.sleep(1.5)
  cli::cli_alert_info(
    "Creating the group names as HTML in {.path _ignore/group-names/}"
  )
  group_names_as_strips_html(
    group_names,
    number_participants = even_number_people
  )
  cli::cli_alert_success(
    "Created the group names as HTML"
  )
  invisible(TRUE)
}

#' Create randomly generated group names.
#'
#' @param number_groups The number of group names to create.
#'
#' @return A character vector of names.
#' @export
#'
#' @examples
#' \dontrun{
#' create_group_names(26)
#' }
create_group_names <- function(number_groups) {
  adjective <- codename::adjectives |>
    subset_words(number_groups)

  animal <- codename::animals |>
    subset_words(number_groups)

  glue::glue("{adjective}-{animal}")
}

subset_words <- function(words, n, min = 4, max = 7) {
  words |>
    unique() |>
    tibble::as_tibble() |>
    dplyr::filter(dplyr::between(nchar(value), min, max)) |>
    dplyr::sample_n(n) |>
    dplyr::pull(value)
}

group_names_to_one_pdf <- function(
  group_names,
  output_dir = here::here("_ignore/group-names")
) {
  fs::dir_create(output_dir)
  group_names |>
    purrr::walk(group_name_to_pdf, output_dir = output_dir)
  single_files <- fs::dir_ls(output_dir, glob = "*.pdf")
  combined_pdf_file <- fs::path(output_dir, "all-groups.pdf")
  Sys.sleep(1)
  pdftools::pdf_combine(single_files, output = combined_pdf_file)
  if (!fs::file_exists(combined_pdf_file)) {
    cli::cli_warn(
      "The file {.path {fs::path_file(combined_pdf_file)}} wasn't actually created, try again?"
    )
  } else {
    cli::cli_alert_success(
      "The group names PDF file ({.path {fs::path_file(combined_pdf_file)}}) was created!"
    )
  }
  fs::file_delete(single_files)
}

group_name_to_pdf <- function(group_name, output_dir) {
  output_file <- stringr::str_replace_all(group_name, "\\.", "-")
  output_file <- fs::path_ext_set(output_file, "pdf")
  quarto::quarto_render(
    input = fs::path_package("samwise", "templates", "group-name-pdf.qmd"),
    output_file = output_file,
    execute_params = list(
      team_name = group_name
    ),
    quiet = TRUE
  )
  fs::file_move(
    fs::path_package("samwise", "templates", output_file),
    fs::path(output_dir, output_file)
  )
}

group_names_as_strips_html <-
  function(
    group_names,
    number_participants,
    output_dir = here::here("_ignore/group-names")
  ) {
    fs::dir_create(output_dir)
    output_file <- "group-names-to-cut.html"
    quarto::quarto_render(
      input = fs::path_package("samwise", "templates", "group-name-strips.qmd"),
      output_file = output_file,
      execute_params = list(
        group_names = group_names,
        number_participants = number_participants
      ),
      quiet = TRUE
    )
    fs::file_move(
      fs::path_package("samwise", "templates", output_file),
      fs::path(output_dir, output_file)
    )
  }
