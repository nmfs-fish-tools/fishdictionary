devtools::load_all(".")
df <- read_in()
require(jsonlite)
json_out_path <- "inst/extdata/top20.json"
write_json(df, path = json_out_path, pretty = T)
in_json <- read_json(json_out_path)
write_json(in_json, json_out_path, pretty = T, auto_unbox = TRUE)

add_object(in_json, list(
    name = "Fork length",
    description = "Length of a fish measured as the distance between the tip of the snout and the center of the fork or \"V\" of the tail. More commonly used compared to total length because the tail of the fish can be damaged and change the total length. The measurement is a \"flat\" measurement usually measured on a board rather than by stretching the tape along the surface of the fish because this can result in a longer measurement for a full-bodied fish.",
     Examples = "",
     `ICES code type` = " ",
     Units = "",
     Rationale = "",
     Alternatives = "",
     `Range of possible values` = ""
))
