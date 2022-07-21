require(shiny)
require(stockassessmentdictionary)
ui <- fluidPage(theme = "nmfs-styles.css",
  uiOutput("choose_topic"),
  wellPanel(uiOutput("documentation")),
  wellPanel(plotOutput("foodweb"))
)