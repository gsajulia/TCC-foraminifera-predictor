server <- function(input, output) {
  callModule(
    module = graph,
    id = "graph_test"
  )
}