library(vroom)

server <- function(input, output) {

  # PREDICT

  # Checkbox decide between new model or default model
  output$newFileName <- renderText({ 
    paste(input$browseNNValues$name)
  })

  output$depthOutput <- renderText({
    paste("RES", input$category, input$depth , sep = "_", collapse = NULL)
  })
  
  # New model #####################################################
  data <- reactive({
    req(input$browseNNValues)

    ext <- tools::file_ext(input$browseNNValues$name)

    inFile <- input$browseNNValues
    
    if (is.null(inFile))
      return(NULL)
    
    switch(ext,
      csv = vroom::vroom(input$browseNNValues$datapath, delim = ","),
      validate("Invalid file; Please upload a .csv")
    )

    df <- read.csv(inFile$datapath,
                header = input$header,
                sep = ",")

    return (df)
  })
  
  # New model Neural network
  # nn <- reactive({
  #     obj = neuralNetwork(
  #     paste("RES", input$category, input$depth , sep = "_", collapse = NULL), data())
  # })

  nn <- eventReactive(input$goButton, {
      showModal(modalDialog("Doing a function", footer=NULL))

      obj = neuralNetwork(
      paste("RES", input$category, input$depth , sep = "_", collapse = NULL), data())
      
      removeModal()

      return (obj)
  })
  
  # New model accuracy
  output$accuracy  <- renderText({
    obj = nn()
    paste(round(obj@accuracy, digits = 2), "%")
  })

  ###########################################################################

  # NEURAL NETWORK INFO
  output$table <- DT::renderDataTable({
    obj = nn()
    return(DT::datatable(obj@table, options = list(scrollX = TRUE)))
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