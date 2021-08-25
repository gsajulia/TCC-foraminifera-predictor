library(fresh)

predict_init_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    width = 12,
    fluidRow(
      width = 12,
      column(
        width = 4,
        tags$div(`class` = "special-title", "Neural Network Model"),
        tags$div(`class` = "medium-title", "Choose the parameter to generete the network"),
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
      ),
      column(
        width = 4,
        align = "center",
        tags$div(
          `class` = "question",
          HTML('<i class="far fa-question-circle"></i>'),
          "More information available in the other tabs of the system"
        ),
        br(),
        tags$div(`class` = "highlight-title", "Model Info"),
        tags$div(`class` = "special-title", "Precision:"),
        br(),
        tags$div(`class` = "special-title", "Accuracy:"),
      ),
      
      column(
        width = 4,
        align = "center",
        class = "box-container",
        tags$div(`class` = "highlight-title", "New model?"),
        tags$div(
          `class` = "medium-title",
          "If you want to create a new model insert a csv file to train a new neural network"
        ),
        
        actionButton(
          icon = icon("plus"),
          class = "second-button",
          ns("browseModel"),
          "Browse"
        ),
      ),
    ),
    fluidRow(
      width = 12,
      align = "center",
      style = 'margin:3vw;',
      tags$div(
        `class` = "question",
        HTML('<i class="far fa-question-circle"></i>'),
        "Hover here to see the expected values or click to download"
      ),
      tags$div(`class` = "medium-title", "Insert .csv file with the foraminifera population"),
      tags$style(".progress-bar{background-color:#3c763d;}"),
      styledFileInput(ns("browseValues"), "Browse", multiple = FALSE,
                              labelIcon = "plus", 
                              progress = FALSE,
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
                                checkboxInput(ns("header"), "Header", TRUE)),
    mainPanel(
      tableOutput(ns("contents"))
    )
  )
}

predict_init <- function(input, output, session) {
  # Input the category of the desire NN
  output$result <- renderText({
    paste("You chose", input$category)
  })
  
  # Create new NN model by inserting csv
  observe({
    if (input$browseModel == 0)
      return()
    
    updateTextInput(session, "path", value = file.choose())
  })
  
  # Insert the initial values with csv
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$browseValues

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath, header = input$header)
  })
}
