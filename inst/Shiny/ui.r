require(shiny)
require(stockassessmentdictionary)
ui <- fluidPage(
  #theme = "nmfs-styles.css",
  includeCSS("../../extra.css"),
  # includeCSS("../../www/extra.css"),
  # tags$head(
  #   tags$style(HTML("
  #     @import url('https://nmfs-fish-tools.github.io/nmfspalette/extra.css');"))
  # ),
  uiOutput("choose_topic"),
  wellPanel(uiOutput("documentation"))
)