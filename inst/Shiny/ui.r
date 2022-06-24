require(shiny)
require(stockassessmentdictionary)
ui <- fluidPage(
  uiOutput("choose_topic"),
  wellPanel(uiOutput("documentation"))
)