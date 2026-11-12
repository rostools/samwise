new_diabetes_path <- here::here("data", "new-diabetes.csv")
if (!fs::file_exists(new_diabetes_path)) {
  download.file(
    "https://r-cubed-advanced.rostools.org/data/new-diabetes.csv",
    destfile = new_diabetes_path,
    # For in Windows users.
    mode = "wb"
  )
}
