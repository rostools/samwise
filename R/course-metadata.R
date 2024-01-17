
list_course_metadata <- function(field = c("id", "url", "github-repo", "name")) {
  courses_path <- fs::path_package(package = "r3admin", "data", "courses.yaml")
  yaml::read_yaml(courses_path)$course |>
    purrr::map_chr(field)
}
