require(shiny)
require(stockassessmentdictionary)
ui <- fluidPage(
  # includeCSS("extra.css"),
  includeCSS("nmfs-styles.css"),
  uiOutput("choose_topic"),
  wellPanel(uiOutput("documentation"))
)