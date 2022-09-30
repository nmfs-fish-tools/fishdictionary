#' Function to add an object to the JSON
#'
#' @param input_list A list of fields corresponding to entries. Must include
#'  \code{name,description,Examples, Units, Rationale, Alternatives, Range of possible values}
#' @return An R list in the JSON format which includes the new term.
#' @export
add_object <- function(input_list) {
  json_obj <- jsonlite::fromJSON(system.file("extdata","top20.json", package = "stockassessmentdictionary"))
  # Return an error if the object already exists
  if (input_list$name %in% sapply(json_obj, get, x = "name")) stop(input_list$name, " already exists.")
  # Validate the json input and return an error if it fails 
  if (jsonvalidate::json_validate(json = jsonlite::toJSON(input_list, auto_unbox = TRUE),
   schema = file.path("inst", "extdata", "schema.json"), verbose = TRUE, error = TRUE)) {
    list_length <- length(json_obj)
    json_obj[[list_length + 1]] <- input_list
    return(json_obj)
  }
}
