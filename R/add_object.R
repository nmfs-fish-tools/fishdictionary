#' Function to add an object to the JSON
#'
#' @param input_list A list of fields corresponding to entries. Must include
#'  \code{name,description,Units}
#' @export
add_object <- function(json_obj, input_list) {
  if (input_list$name %in% sapply(json_obj, get, x = "name")) stop(input_list$name, " already exists.")
  if (jsonvalidate::json_validate(json = jsonlite::toJSON(input_list, auto_unbox = TRUE), schema = file.path("inst", "extdata", "schema.json"), verbose = TRUE, error = TRUE)) {
    list_length <- length(json_obj)
    json_obj[[list_length + 1]] <- input_list
    return(json_obj)
  }
}
