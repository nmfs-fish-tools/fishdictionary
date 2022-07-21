# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  require(jsonlite)
  library(stockassessmentdictionary)
  require(ggplot2)
  require(shiny)

 tmp <- tempfile()
  onSessionEnded(function() {
    unlink(tmp) })

  rd_database <- reactive({
    tools::Rd_db(package = "stockassessmentdictionary")
  })

  output$choose_topic <- renderUI({
    selectInput("Term", "Select term or function",
    sub(".Rd", "", names(rd_database())))
  })

  output$documentation <- renderUI({
    rdfile <- paste0(input$topic, ".Rd")
    print(rdfile)
    req(rdfile %in% names(rd_database()))
    tools::Rd2HTML(rd_database()[[rdfile]], tmp,
    package = "stockassessmentdictionary")
    htmltools::includeHTML(tmp)
  })
}
