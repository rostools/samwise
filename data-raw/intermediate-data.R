library(fs)
library(here)
options(timeout = 120)

mmash_link <-
  "https://physionet.org/static/published-projects/mmash/multilevel-monitoring-of-activity-and-sleep-in-healthy-people-1.0.0.zip"

download.file(mmash_link, destfile = here("data-raw/mmash-data.zip"))

# usethis::use_git_ignore("data-raw/mmash-data.zip")

unzip(here("data-raw/mmash-data.zip"),
  exdir = here("data-raw"),
  junkpaths = TRUE
)

Sys.sleep(1)

unzip(here("data-raw/MMASH.zip"),
  exdir = here("data-raw")
)

file_delete(here(c(
  "data-raw/MMASH.zip", "data-raw/SHA256SUMS.txt", "data-raw/LICENSE.txt"
)))

file_move(here("data-raw/DataPaper"), here("data-raw/mmash"))

# usethis::use_git_ignore("data-raw/mmash/")

zip::zip()
