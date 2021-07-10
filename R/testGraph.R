library(fresh)

graph_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # A static valueBox
      valueBox(10 * 2, "New Orders", icon = icon("credit-card"), color = "blue"),
      
      # Dynamic valueBoxes
      valueBoxOutput(ns("progressBox")),
      
    )
  )
}

graph <- function(input, output, session) {
  output$progressBox <- renderValueBox({
    valueBox(
      300, "Progress", icon = icon("list"),
      color = "aqua"
    )
  })
}