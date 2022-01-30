library(vroom)
library(devtools)
library("rpart")
library("rpart.plot")
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
load("./data.RData")

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
  
  # Downloadable csv of selected dataset ----
  output$downloadDataModel <- downloadHandler(
    filename = function() {
      paste("last_forams_data_clean", ".csv", sep = "")
    },
    content = function(file) {
      file.copy("last_forams_data_clean_last.csv", file)
    }
  )
  
  output$downloadDataPredict <- downloadHandler(
    filename = function() {
      paste("forams_values", ".csv", sep = "")
    },
    content = function(file) {
      file.copy("forams_values.csv", file)
    }
  )
  
  #op1
  output$checkboxOption <- renderText({ 
    paste(input$browseNNValues$name)
  })

  observe({
    # WARNING IN CREATE NEW MODEL
    if(is.null(input$browseNNValues) && input$rb=="new")
      output$inputWarning <- renderText({
        paste("ERROR:  You should Browse a new model")
      })
    else
      output$inputWarning <- renderText({
        paste("")
      })

    # ACTION BUTTON
    if(is.null(input$browseNNValues) && input$rb=="new")
      output$predictButtonText <- renderText({
        paste("CREATE & LOAD MODEL")
      })
    else if(input$rb=="new")
      output$predictButtonText <- renderText({
        paste("CREATE & LOAD MODEL")
      })
    else
      output$predictButtonText <- renderText({
        paste("LOAD MODEL")
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
      showModal(modalDialog("Loading...", footer=NULL))

      if(input$rb=="new" && is.null(input$browseNNValues)) {
        removeModal()
        return (NULL)
      }
      else if(input$rb=="new")
        obj = neuralNetwork(
        paste("RES", input$category, input$depth , sep = "_", collapse = NULL), data())
      else
        obj = useNeuralNetwork(paste("RES", input$category, input$depth , sep = "_", collapse = NULL))

      removeModal()

      return (obj)
  })
    
    output$metrics <- renderUI({
          obj = nn()
          
          return (column(
              width=12,
              column(
                width= 6,
                div(
                  div(
                  `class` = "special-title info-box",
                  div("MEAN SQUARED ERROR:", 
                  span(style = "font-weight: bold",obj$mse)),
                  br(),
                  div(`class` = "small-title",
                  "MSE is perhaps the most popular metric used for regression problems. Using this formula we find the mean square error between the predicted and actual values.")
                  )
                ),
                br(),
                div(
                  div(
                  `class` = "special-title info-box",
                  div("MEAN ABSOLUTE ERROR:", 
                  span(style = "font-weight: bold",obj$mae)),
                  br(),
                  div(`class` = "small-title",
                  "MAE is a metric that finds the mean absolute distance between predicted and desired values.")
                  )
                ),
              ),
              column(
                width= 6,
              div(
                div(
                `class` = "special-title info-box",
                div("ROOT MEAN SQUARED ERROR:", 
                span(style = "font-weight: bold",obj$rmse)),
                br(),
                div(`class` = "small-title",
                "It is similar to MSE, but the square root returns the scale of the squared errors.")
                )
              ),
              br(),
              div(
                div(
                `class` = "special-title info-box",
                div("RELATIVE ABSOLUTE ERROR:", 
                span(style = "font-weight: bold",obj$rae)),
                br(),
                div(`class` = "small-title",
                "It is the MAE relativized by dividing the MAE using the average as the value subtracted from the sum.")
                )
              )
              )
          ))
    })

        output$infoLoaded <- renderUI({
          obj = nn()
          loaded = !!obj$mse
          if(loaded) {
            return (div(
                div(`class` = "loaded","Model loaded!"),
            ))
          } else {
              return (div(
                div(`class` = "error","Error, try to add the model again"),
            ))
          }
    })

  ###########################################################################
        
  # Predict #################################################################
  # Insert the initial values with csv
    output$downloadData <- downloadHandler(
      filename = function(){"resultForam.csv"}, 
      content = function(fname){
        obj = nn()
        predict = neuralnet::compute(obj$nn, dfValues);
        
        desnormalize <- function(normalizedValue, originalValue) {
          z = normalizedValue * (max(originalValue) - min(originalValue)) + min(originalValue)
          return(z)
        }
        write.csv(data.frame(cbind(dfValues, Valor_predito=desnormalize(predict$net.result, obj$cleanDf))), fname)
      }
    )

    output$downloadPredicted <- renderUI({
      inFile <- input$browseValues

      if (is.null(inFile))
         NULL
      else div(style="padding-bottom: 20px", downloadButton("downloadData", "Download"))
    })
        
  output$valuesTable <- DT::renderDataTable({
    inFile <- input$browseValues
    
    if (is.null(inFile))
      return(NULL)
    
    dfValues <<- read.csv(inFile$datapath,
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


  # Anova tree ################################################################
    treeFunction <<- eventReactive(input$goButton, {
      if(input$rb=="new") {
        obj = anovaTree(
        paste("RES", input$category, input$depth , sep = "_", collapse = NULL), data())
      }
      else {
        obj = useNeuralNetwork(paste("RES", input$category, input$depth , sep = "_", collapse = NULL))
        obj = obj$foramTree
      }

      return (obj)
  })

  output$plotTree <- renderPlot({
    tree = treeFunction()

    # Result Plot
    rpart.plot(tree, type = 3, digits = 2)
  })
  #############################################################################
  # callModule(
  #   module = neural_network,
  #   id = "init_neural_network"
  # )
}