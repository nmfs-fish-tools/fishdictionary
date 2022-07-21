require(shiny)
require(stockassessmentdictionary)
ui <- fluidPage(theme = "../www/nmfs-styles.css",
  uiOutput("choose_topic"),
  wellPanel(uiOutput("documentation"))
)