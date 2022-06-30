#' Run the data dictionary app
#'
#' Run the data dictionary app
#' @export
#' @param browse Logical. Use browser for running Shiny app.
#' @examples
#' \dontrun{
#' if(require(shiny)){
#'    shiny_dd()
#' }
#' }
shiny_dd <- function(browse=TRUE){
  shiny::runApp(system.file('Shiny', package='stockassessmentdictionary'), 
  launch.browser = browse, display.mode =  "showcase")
}