devtools::load_all(".")
df <- read_in()
require(jsonlite)
library(jsonvalidate)

# Load json schema
schema_path <- file.path("inst", "extdata", "schema.json")

# Load top20.json
json_out_path <- "inst/extdata/top20.json"
jsonlite::write_json(df, path = json_out_path, pretty = T)
in_json <- jsonlite::read_json(json_out_path)
jsonlite::write_json(in_json, json_out_path, pretty = T, auto_unbox = TRUE)

# Validate top20.json
for (i in 1:length(in_json)){
    
    data <- jsonlite::toJSON(in_json[[i]], auto_unbox=TRUE)
    cat(jsonvalidate::json_validate(json=data, schema = schema_path, verbose = TRUE, error = TRUE))
    print(in_json[[i]]$name)
    
}

# Create fork_length 
fork_length <- list(
    name = "Fork length",
    description = "Length of a fish measured as the distance between the tip of the snout and the center of the fork or \"V\" of the tail. More commonly used compared to total length because the tail of the fish can be damaged and change the total length. The measurement is a \"flat\" measurement usually measured on a board rather than by stretching the tape along the surface of the fish because this can result in a longer measurement for a full-bodied fish.",
    Examples = "",
    `ICES code type` = " ",
    Units = "",
    Rationale = "",
    Alternatives = "",
    `Range of possible values` = ""
)

# Validate fork_length, if returns TRUE, add fork_length to in_json
jsonvalidate::json_validate(json=jsonlite::toJSON(fork_length, auto_unbox=TRUE), schema = schema_path, verbose = TRUE, error = TRUE)

in_json <- add_object(in_json, fork_length)

# fork_length example with missing name 
fork_length_noName <- fork_length
fork_length_noName$name <- ""

# Validate fork_length_fork_length_noName
jsonvalidate::json_validate(json=jsonlite::toJSON(fork_length_noName, auto_unbox=TRUE), schema = schema_path, verbose = TRUE, error = TRUE)
# Error: 1 error validating json:
# - data.name: has less length than allowed

in_json <- add_object(in_json, fork_length_noName)

# Update fork_length_noName and add a name
fork_length_fixed <- fork_length
jsonvalidate::json_validate(json=jsonlite::toJSON(fork_length_fixed, auto_unbox=TRUE), schema = schema_path, verbose = TRUE, error = TRUE)
in_json <- add_object(in_json, fork_length_fixed)

# Update name of fork_length_fixed
fork_length_fixed$name <- "fork_length_fixed"
jsonvalidate::json_validate(json=jsonlite::toJSON(fork_length_fixed, auto_unbox=TRUE), schema = schema_path, verbose = TRUE, error = TRUE)
in_json <- add_object(in_json, fork_length_fixed)

