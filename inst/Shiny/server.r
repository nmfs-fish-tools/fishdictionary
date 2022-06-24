# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  require(jsonlite)
  library(stockassessmentdictionary)
  require(ggplot2)
output$choose_topic <- renderUI({
    choices <- list.files(system.file("html", package = "stockassessmentdictionary"))
    selectInput("topic", "select topic", choices)
  })

  output$documentation <- renderUI({
    includeHTML(
      system.file(paste0("html/", req(input$topic)), package = "stockassessmentdictionary")
    )
  })
}