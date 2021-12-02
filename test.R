source("R/read_in.R")
df <- read_in()
require(jsonlite)
write_json(df, path="top20.json", pretty=T)
