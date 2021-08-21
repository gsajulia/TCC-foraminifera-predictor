server <- function(input, output) {
  callModule(
    module = predict_init,
    id = "init_prediction"
  )
}