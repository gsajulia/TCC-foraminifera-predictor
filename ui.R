library(shinydashboard)
library(fresh)
library(shinycssloaders)

my_theme <- create_theme(adminlte_color(light_blue = "#28A390"),
                         adminlte_sidebar(dark_bg = "#212936"))

options(
  spinner.color = "#53b5a6",
  spinner.color.background = "#ffffff",
  spinner.size = 1
)

ui <- dashboardPage(
  dashboardHeader(title = "Foraminiferal"),
  dashboardSidebar(
    collapsed = FALSE,
    sidebarMenu(
      tags$div(`class` = "subtitle", "Predictions"),
      hr(),
      menuItem(
        "Predict",
        tabName = "predict",
        icon = icon("searchengin")
      ),
      menuItem(
        "Neural Network Info",
        icon = icon("brain"),
        tabName = "neuralNetwork"
      ),
      menuItem("Graphs",
               icon = icon("chart-bar"),
               tabName = "graphs"),
      hr(),
      menuItem(
        "Anova Tree",
        icon = icon("network-wired"),
        tabName = "anova"
      ),
      menuItem("Help",
               icon = icon("question"),
               tabName = "help")
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    tags$style(
      "@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"
    ),
    # Including css
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "styles.css"),
    # Including fonts
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Qahiri&display=swap"),
    tabItems(
      # Predic tab
      tabItem(
        tabName = "predict",
        box(
          width = 12,
          fluidPage(
            width = 12,
            fluidRow(
              width = 10,
              style = "display: flex;
                        background-color: #f2fcf2;
                        justify-content: space-around;
                        padding: 25px;
                        border-radius: 15px;",
              column(
                width = 4,
                tags$div(
                  class = "box-container",
                  tags$div(`class` = "highlight-title", "New model?"),
                  tags$div(
                    `class` = "medium-title",
                    "If you want to create a new model insert a csv file to train a new neural network"
                  ),
                  
                  styledFileInput(
                    "browseNNValues",
                    "Browse",
                    multiple = FALSE,
                    btnStyle = "file-btn-light",
                    labelIcon = "plus",
                    progress = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  ),
                ),
                br(),
                div(`class` = "special-title medium-title test-title",
                    "Data to test", ),
                div(
                  `class` = "test-title",
                  span(`class` = "medium-title-custom-border",
                       "Create model"),
                  downloadButton("downloadDataModel", "Download")
                ),
                div(
                  span(`class` = "medium-title-custom-border",
                       style = 'padding-right:50px;',
                       "Predict"),
                  downloadButton("downloadDataPredict", "Download")
                ),
                br(),
                tags$div(
                  `class` = "question",
                  HTML('<i class="far fa-question-circle"></i>'),
                  "More information available in the other tabs of the system"
                ),
              ),
              column(
                width = 4,
                tags$div(
                  tags$div(`class` = "special-title", "Neural Network Model"),
                  tags$div(`class` = "medium-title", "Choose the parameter to generate the network"),
                  tags$div(
                    `class` = "checkbox-input-container",
                    "Entry data",
                    #op2 TODO
                    #   radioButtons(
                    #   "rb",
                    #   "Choose one:",
                    #   choices = verbatimTextOutput("checkboxOption"),
                    # ),
                    #op1
                    radioButtons(
                      "rb",
                      "Choose one:",
                      choiceNames = list(
                        tags$span("Default Models"),
                        textOutput(outputId = "checkboxOption", container = span)
                      ),
                      choiceValues = list("default", "new")
                    ),
                    # uiOutput(outputId = "selectB"),
                    # uiOutput('checkboxOptions'),
                    span(textOutput("inputWarning"), style = "color:red")
                    # conditionalPanel(condition = "output.fileUploaded && input.rb==='new'",
                    #                  span(textOutput("inputWarning"), style = "color:red")),
                  ),
                  
                  br(),
                  selectInput(
                    "category",
                    "Model Period",
                    list("Annual",
                         "jan-mar",
                         "jul-sep")
                  ),
                  selectInput(
                    "depth",
                    "Model Depth",
                    list("0m",
                         "50m",
                         "75m",
                         "100m",
                         "0-75m",
                         "0-100m",
                         "0-200m")
                  ),
                  tags$div(
                    `class` = "action-button-container",
                    "Your table Choose",
                    textOutput("depthOutput"),
                    br(),
                    uiOutput(outputId="infoLoaded"),
                    br(),
                    actionButton("goButton", textOutput("predictButtonText"), class = "file-btn-main"),
                  ),

                )
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
              styledFileInput(
                "browseValues",
                "Browse and Predict",
                placeholder = "No file selected",
                multiple = FALSE,
                labelIcon = "plus",
                progress = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),
            ),
            uiOutput(outputId = "downloadPredicted"),
            shinycssloaders::withSpinner(
              DT::dataTableOutput(outputId = "valuesTable", width = "74vw"),
              type = 2
            )
          ),
          
          title = "Predict",
          solidHeader = TRUE,
          
        )
      ),
      # Neural network tab
      tabItem(
        tabName = "neuralNetwork",
        box(
          width = 800,
          title = "Neural Network Information",
          solidHeader = TRUE,
          shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "table", width = "74vw"), type =
                                         2)
        )
      ),
      tabItem(
        tabName = "graphs",
        
        box(
          width = 800,
          height = 1700,
          title = "Graphs",
          solidHeader = TRUE,
          
          plotOutput("plot1"),
          br(),
          br(),
          plotOutput("plot2"),
        )
      ),
      tabItem(
        tabName = "anova",
        box(
          width = 800,
          height = 1700,
          title = "Anova Tree",
          solidHeader = TRUE,
          plotOutput("plotTree"),
          div(`class` = "highlight-title", "Predicted by Anova Tree"),
          DT::dataTableOutput(outputId = "foramTreee", width = "74vw")
        )
      ),
      tabItem(
        tabName = "help",
        box(
          width = 800,
          height = 1700,
          title = "Help",
          solidHeader = TRUE,
          column(
            div(`class` = "small-title",
                "See more info about the Neural Network metrics below, this is a Regression problem so the metrics will be different types of errors, the smaller the error, the better the model."),
            br(),
            div(`class` = "small-title", "More info is available in: ", 
                a("Regression Errors", href="https://machinelearningmastery.com/regression-metrics-for-machine-learning/#:~:text=There%20are%20three%20error%20metrics,Mean%20Absolute%20Error%20(MAE)", target="_blank"), " & ",
                a("Anova Tree", href="https://www.youtube.com/watch?v=r1ueoHA_KCQ&t=68s", target="_blank")),
            width = 12,
            align = "right",
            br(),
            tags$div(`class` = "highlight-title", "Model Info"),
            uiOutput(outputId="metrics"),
            br(),
            column(
              width=12,
            div(
              style="margin-top: 20px;",
              `class` = "info-box-tree",
            div(style="text-align: left; padding-bottom: 20px;", `class` = "highlight-title", "Anova Tree"),
            div(`class` = "small-title", "Analysis of variance (ANOVA) is a statistical test for detecting differences in the group mean when there is one parametric dependent variable & one or more independent variable.")))
            ),
        )
      )
    )
  )
)