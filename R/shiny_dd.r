#' Run the fish dictionary app
#'
#' Run the fish dictionary app
#' @export
#' @param browse Logical. Use browser for running Shiny app.
#' @examples
#' \dontrun{
#' if (require(shiny)) {
#'   shiny_dd()
#' }
#' }
shiny_dd <- function(browse = TRUE) {
  shiny::runApp(system.file("Shiny", package = "fishdictionary"),
    launch.browser = browse
  )
}
