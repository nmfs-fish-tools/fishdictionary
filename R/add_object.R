#' Function to add an object to the JSON
#'
#' @param input_list A list of fields corresponding to entries. Must include
#'  \code{name,description,Units}
#' @export
add_object <- function(json_obj, input_list) {
  list_length <- length(json_obj)
  json_obj[[list_length + 1]] <- input_list
  return(json_obj)
}

