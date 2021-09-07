library(fresh)

neural_network_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    div("ta funfando?"),
    DT::dataTableOutput(outputId = ns("table"))
  )
}

neural_network <- function(input, output, session, objResult) {
  # Create new NN model by inserting csv
  output$table <- DT::renderDataTable({
    #Retorna a tabela paginada
    return(DT::datatable(objResult@table))
  })
}