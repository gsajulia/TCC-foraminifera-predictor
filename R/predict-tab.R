library(fresh)

predict_init_UI <- function(id) {
  ns <- NS(id)
  fluidPage(width = 12,
    tags$div(`class` = "special-title", "Neural Network Model"),
    tags$div(`class` = "medium-title", "Choose the parameter to generete the network"),
    br(),
    br(),
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
}

predict_init <- function(input, output, session) {
  output$result <- renderText({
    paste("You chose", input$category)
  })
}
