require(shiny)
require(fishdictionary)
ui <- fluidPage(
  # includeCSS("extra.css"),
  includeCSS("nmfs-styles.css"),
  uiOutput("choose_topic"),
  wellPanel(uiOutput("documentation"))
)