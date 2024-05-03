library(tidyverse)

# https://dailies.rstudio.com/json-api/

tail(rversions::r_versions(), n = 3)

rjson::fromJSON(file = "https://dailies.rstudio.com/rstudio/latest/index.json")

"https://www.rstudio.com/wp-content/downloads.json"
