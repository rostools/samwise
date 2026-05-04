# Source this file to download the DIME dataset and unzip it to the
# `data-raw/dime/` directory.

# TODO: Manually download this file from the link below and save it to `data-raw/dime.zip`.
# https://drive.proton.me/urls/14BXRJYXXW#fk6PbSSgVUwD
unzip(
  here("data-raw/dime.zip"),
  exdir = here("data-raw/dime/")
)
