server <- function(input, output) {
  # Input the category of the desire NN
  output$result <- renderText({
    paste("You chose", input$category)
  })
  
  # Create new NN model by inserting csv
  output$table <- DT::renderDataTable({
    inFile <- input$browseNNValues
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath,
                   header = input$header,
                   sep = ",")
    
    obj = neuralNetwork(null, null)
    
    output$accuracy  <- renderText({ 
      paste(round(obj@accuracy, digits = 2), "%")
    })

    #Retorna a tabela paginada
    return(DT::datatable(obj@table))
  })
  
  # Insert the initial values with csv
  output$attributes <- renderUI({
    inFile <- input$browseValues
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath,
                   header = input$header,
                   sep = ";")
    
    #Names of the attributes from csv
    dfNames <- names(df)
    
    #NA coluns receive 0
    df[is.na(df)] = 0
    
    #Return of population names
    return(lapply(1:length(dfNames), function(i) {
      div(dfNames[i])
    }))
    
  })
  
  # callModule(
  #   module = neural_network,
  #   id = "init_neural_network"
  # )
}