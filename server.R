library(vroom)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

server <- function(input, output) {

  # PREDICT

  # Checkbox decide between new model or default model
   #op2 TODO
  # output$checkboxOption <- renderPrint({
  #   if(input$browseNNValues$name)
  #     return(list("Default Models", input$browseNNValues$name))
  #   else
  #     return(list("Default Models"))
  # })

#   output$selectB <- renderUI({
#   if(req(input$browseNNValues$name)) ?
#     options <- c("Default Models", input$browseNNValues$name) 
#   else 
#     options <- c("Default Models") 

#   return(
#   radioButtons('selectB', 'Select Letter', 
#                 choices = options,
#                 inline = TRUE))
# })

  #op1
  output$checkboxOption <- renderText({ 
    paste(input$browseNNValues$name)
  })

  observe({
    if(is.null(input$browseNNValues) && input$rb=="new")
      output$inputWarning <- renderText({
        paste("ERROR:  You should Browse a new model")
      })
    else
      output$inputWarning <- renderText({
        paste("")
      })
  })

  # Input the category of the desire NN
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
                header = TRUE,
                sep = ",")

    return (df)
  })
  
  # New model Neural network
  # nn <- reactive({
  #     obj = neuralNetwork(
  #     paste("RES", input$category, input$depth , sep = "_", collapse = NULL), data())
  # })

  nn <<- eventReactive(input$goButton, {
      showModal(modalDialog("Doing a function", footer=NULL))

      obj = neuralNetwork(
      paste("RES", input$category, input$depth , sep = "_", collapse = NULL), data())
      
      removeModal()

      return (obj)
  })
  
  # New model accuracy
  output$accuracy  <- renderText({
    obj = nn()
    paste(round(obj$accuracy, digits = 2), "%")
  })


  # New model accuracy
  output$precision  <- renderText({
    obj = nn()
    paste(round(obj$precision, digits = 2), "%")
  })


  ###########################################################################

  # Predict #################################################################
  # Insert the initial values with csv
  output$valuesTable <- DT::renderDataTable({
    inFile <- input$browseValues
    
    if (is.null(inFile))
      return(NULL)
    
    dfValues <- read.csv(inFile$datapath,
                   header = TRUE,
                   sep = ",")
    
    #Names of the attributes from csv
    dfNames <- names(dfValues)
    
    #NA coluns receive 0
    dfValues[is.na(dfValues)] = 0
    
    obj = nn()
    predict = neuralnet::compute(obj$nn, dfValues);

    desnormalize <- function(normalizedValue, originalValue) {
        z = normalizedValue * (max(originalValue) - min(originalValue)) + min(originalValue)
        return(z)
    }

    return(DT::datatable(cbind(dfValues, Valor_predito=desnormalize(predict$net.result, obj$cleanDf)), options = list(scrollX = TRUE)))
  })


  ###########################################################################

  # NEURAL NETWORK INFO #####################################################
  output$table <- DT::renderDataTable({
    obj = nn()
    return(DT::datatable(obj$table, options = list(scrollX = TRUE)))
  })
  
  # Graphs ##################################################################

  output$plot1 <- renderPlot({
    obj = nn()
    predict = neuralnet::compute(obj$nn, obj$test);
    # Result Plot
    plot(obj$test[paste("RES", input$category, input$depth , sep = "_", collapse = NULL)][,1], predict$net.result[,1],col='red',main='Real vs predicted NN', xlab="Real", ylab="DNN")
    abline(0,1,lwd=2)
  })

  output$plot2 <- renderPlot({
    obj = nn()
    plot.nnet(obj$nn)
  }, height = 1000)

  # callModule(
  #   module = neural_network,
  #   id = "init_neural_network"
  # )
}