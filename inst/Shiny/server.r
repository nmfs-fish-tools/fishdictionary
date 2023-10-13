# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  require(jsonlite)
  #library(fishdictionary)
  require(ggplot2)
  require(shiny)

 tmp <- tempfile()
  onSessionEnded(function() {
    unlink(tmp) })

  rd_database <- reactive({
    tools::Rd_db(package = "fishdictionary")
  })

  output$choose_topic <- renderUI({
    selectInput("Term", "Select term or function",
    sub(".Rd", "", names(rd_database())))
  })

  output$documentation <- renderUI({
    rdfile <- paste0(input$Term, ".Rd")
    req(rdfile %in% names(rd_database()))
    tools::Rd2HTML(rd_database()[[rdfile]], tmp,
    package = "fishdictionary")
    htmltools::includeHTML(tmp)})

  output$foodweb <- renderPlot({
    mvbutils::foodweb(where = "package:fishdictionary",
     prune = input$Term)
  })
}