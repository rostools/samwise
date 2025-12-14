save_to_csv <- function(data, path) {
  fs::dir_create(fs::path_dir(path))
  data |>
    readr::write_csv(here::here(path))
  path
}

create_path_from_columns <- function(columns) {
  columns |>
    as.list() |>
    purrr::pmap(fs::path) |>
    purrr::map_chr(~ usethis::proj_path("inst", "extdata", "surveys", .x))
}

save_responses_to_csv <- function(data, columns) {
  data_to_save <- data |>
    tidyr::nest(.by = tidyselect::all_of(columns)) |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(columns), as.character)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      path = create_path_from_columns(dplyr::c_across(tidyselect::all_of(
        columns
      )))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(data, path)

  purrr::map2_chr(
    data_to_save$data,
    fs::path_ext_set(data_to_save$path, "csv"),
    save_to_csv
  ) |>
    fs::path_rel(usethis::proj_path(".")) |>
    as.character()
}
