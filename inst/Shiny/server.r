# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  require(jsonlite)
  library(stockassessmentdictionary)
  require(ggplot2)
  require(shiny)

 tmp <- tempfile()
  onSessionEnded(function(){ unlink(tmp) })

  RdDatabase <- reactive({
    tools::Rd_db(package = "stockassessmentdictionary")
  })

  output$choose_topic <- renderUI({
    selectInput("Term", "Select term or function", 
    sub(".Rd", "", names(RdDatabase())))
  })

  output$documentation <- renderUI({
    rdfile <- paste0(input$topic, ".Rd")
    req(rdfile %in% names(RdDatabase()))
    tools::Rd2HTML(RdDatabase()[[rdfile]], tmp, no_links = TRUE, 
    package = "stockassessmentdictionary")
    htmltools::includeHTML(tmp)
  })
}