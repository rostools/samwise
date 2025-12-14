#' Read in the workshop's metadata (as a YAML) saved in the package's `data/`
#' folder.
#'
#' @return A (heavily nested) list.
#' @export
#'
#' @examples
#' read_workshop_metadata()
read_workshop_metadata <- function() {
  path <- fs::path_package(
    package = "samwise",
    "extdata",
    "workshops.yaml"
  )
  yaml::read_yaml(path)$workshop
}

#' List all the IDs for the workshops.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' list_workshop_ids()
list_workshop_ids <- function() {
  read_workshop_metadata() |>
    purrr::map_chr("id") |>
    sort()
}

#' List all the metadata fields available in the first-level for each workshop.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' list_workshop_metadata_fields()
list_workshop_metadata_fields <- function() {
  read_workshop_metadata() |>
    purrr::pluck(1) |>
    names() |>
    stringr::str_subset("id", negate = TRUE)
}

#' Metadata on the dates the workshops have started on.
#'
#' @inheritParams get_workshop_metadata_field
#'
#' @return Character vector of dates.
#' @export
#'
#' @examples
#' get_workshop_dates("inter")
#' get_workshop_dates("intro")
#' get_workshop_dates("adv")
get_workshop_dates <- function(id) {
  get_workshop_metadata_field(id, "events") |>
    purrr::map_chr(~ purrr::pluck(.x, "date")) |>
    sort()
}

#' General purpose function
#'
#' @param id The ID of the workshop, found by running [list_workshop_ids()].
#' @param field The "key" value (field) for the ID of the workshop, found by running [list_workshop_metadata_fields()].
#'
#' @return A (nested) list.
#' @export
#'
#' @examples
#'
#' get_workshop_metadata_field("intro", "events")
#' get_workshop_metadata_field("intro", "date")
get_workshop_metadata_field <- function(id, field) {
  id <- rlang::arg_match(id, list_workshop_ids())
  field <- rlang::arg_match(field, list_workshop_metadata_fields())
  read_workshop_metadata() |>
    purrr::keep(~ .x$id == id) |>
    purrr::pluck(1, field)
}

#' Get the dates for the next workshop.
#'
#' @inheritParams get_workshop_metadata_field
#'
#' @return A character vector.
#' @export
#'
#' @examples
#'
#' get_upcoming_workshop_dates("intro")
#' get_upcoming_workshop_dates("inter")
#' get_upcoming_workshop_dates("adv")
get_upcoming_workshop_dates <- function(id) {
  get_workshop_dates(id = id) |>
    purrr::keep(~ .x >= lubridate::today())
}

#' Get the ID of the workshop that is upcoming.
#'
#' @return A character scalar.
#' @export
#'
#' @examples
#' get_upcoming_workshop()
get_upcoming_workshop <- function() {
  workshop_metadata <- read_workshop_metadata()

  upcoming <-
    workshop_metadata |>
    purrr::map("events") |>
    purrr::map_depth(2, "date", .ragged = TRUE) |>
    purrr::map(unlist) |>
    purrr::map_chr(\(x) {
      x <- x[x >= lubridate::today()]
      if (!length(x)) {
        # When no date exists, set to NA
        NA
      } else {
        # Get the nearest date from future dates
        min(x)
      }
    }) |>
    lubridate::as_date()

  id <- NA
  # If there are any upcoming dates
  if (any(!is.na(upcoming))) {
    # The date should be the soonest of the upcoming dates
    upcoming <- upcoming == min(upcoming, na.rm = TRUE)
    id <- read_workshop_metadata() |>
      purrr::map("id") |>
      purrr::keep(upcoming) |>
      unlist()
  }

  id
}

get_workshop_repo <- function(id) {
  get_workshop_metadata_field(id, "repo")
}
