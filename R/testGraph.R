library(fresh)

predict_init_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$div(`class` = "special-title", "Neural Network Model"),
    "Choose the parameter to generete the network",
    br(),
    br(),
    column(
      width = 6,
      selectInput(
        ns("category"),
        "Model Category",
        list(
          `Basins` = list("NY", "NJ", "CT"),
          `Periods` = list("WA", "OR", "CA"),
          `Depth` = list("MN", "WI", "IA")
        )
      ),
      textOutput(ns("result"))
    )
  )
}

predict_init <- function(input, output, session) {
  output$result <- renderText({
    paste("You chose", input$category)
  })
}