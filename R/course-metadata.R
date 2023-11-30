
list_courses <- function() {
  courses_path <- fs::path_package(package = "r3admin", "data", "courses.yaml")
  yaml::read_yaml(courses_path)$course |>
    purrr::pluck("")
}
