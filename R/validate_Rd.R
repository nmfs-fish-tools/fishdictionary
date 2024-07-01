#' Function to validate terms in the dictionary.
#'
#' The dictionary terms are written in R documentation (Rd) format.
#' This function reads the Rd file using the `tools::parse_Rd` function
#' validate the information in each section (e.g., examples, rationale,
#' range of possible values, etc.).
#'
#' @param Rd_file a Rd filename to use as input.
#' @return a message if the term passes all validations.
#' @import tools
#' @export
validate_Rd <- function(Rd_file) {
  if (is.null(Rd_file)) stop("Please provide the path to the R documentation (Rd) file.")

  Rd <- tools::parse_Rd(Rd_file)
  tags <- lapply(Rd, attr, "Rd_tag")

  # Validate name
  name <- Rd[[which(tags == "\\name")]]
  if (is.null(name[[1]][1])) stop("Name is not provided. This should be the basename of the Rd file.")

  # Validate title
  title <- Rd[[which(tags == "\\title")]]
  if (is.null(title[[1]][1])) stop("Title is not provided. Title should be capitalized and not end in a period.")

  # Validate description
  description <- Rd[[which(tags == "\\description")]]
  if (gsub("[\r\n]", "", description[[2]][1]) == title[[1]][1] |
    is.null(description[[1]][1])) {
    stop("Description is not provided. This should be capitalized and not end in a period.")
  }

  # Extract format section for validating examples, rationale, alternatives, range of possible values, and units
  format <- Rd[[which(tags == "\\format")]][[2]]
  items <- c("Examples", "Rationale", "Alternatives", "Range of possible values", "Units")
  missing_items <- items[!is.element(items, unlist(format))]
  if (any(!is.element(items, unlist(format)))) stop(paste("Missing", paste(missing_items, collapse = ", ")))
  
  # Validate examples
  if (format[[2]][[1]][[1]][1] == "Examples" &
    length(format[[2]][[2]]) == 0) {
    stop("Examples are not provided. Use NA if there is no input for examples.")
  }

  # Validate rationale
  if (format[[4]][[1]][[1]][1] == "Rationale" &
    length(format[[4]][[2]]) == 0) {
    stop("Rationale is not provided.")
  }

  # Validate alternatives
  if (format[[6]][[1]][[1]][1] == "Alternatives" &
    length(format[[6]][[2]]) == 0) {
    stop("Alternatives are not provided. Use NA if there is no input for alternatives.")
  }

  # Validate range of possible values
  if (format[[8]][[1]][[1]][1] == "Range of possible values" &
    length(format[[8]][[2]]) == 0) {
    stop("Range of possible values is not provided. Use NA if there is no input for range of possible values.")
  }

  # Validate range of possible values
  if (format[[10]][[1]][[1]][1] == "Units" &
    length(format[[10]][[2]]) == 0) {
    stop("Units are not provided. Use NA if there is no input for units.")
  }


  # Return a message if the term passes all validations
  message(paste("Term", name[[1]][1], "has passed all validations!"))
}
