library(shinydashboard)

my_theme <- create_theme(adminlte_color(light_blue = "#28A390"),
                         adminlte_sidebar(dark_bg = "#212936"))


# Put them together into a dashboardPage
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
        icon = icon("network-wired"),
        tabName = "neuralNetwork"
      ),
      menuItem("Graphs", icon = icon("chart-bar"), tabName = "graphs"),
      hr(),
      menuItem("Help", icon = icon("question"), tabName = "help")
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    tags$style(
      "@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"
    ),
    # Including css
    tags$link(rel =  "stylesheet",
              type = "text/css",
              href = "styles.css"),
    # Including fonts
    tags$link(rel =  "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Qahiri&display=swap"),
    tabItems(
      tabItem(
        tabName = "predict",
        box(
          width = 12,
          column(width = 4,
                 predict_init_UI("init_prediction")),
          
          title = "Predict",
          solidHeader = TRUE,
          column(
            width = 4,
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
            tags$div(`class` = "highlight-title", "New model?"),
            tags$div(`class` = "medium-title", "If you want to create a new model insert a csv file to train a new neural network"),
            br(),
            actionButton("browse", "Browse"),
          )
          
        )
      ),
      
      tabItem(
        tabName = "neuralNetwork",
        h2("Widgets tab content"),
        box(
          title = "Inputs",
          solidHeader = TRUE,
          "Box content here",
          br(),
          "More box content"
        )
      )
    )
  )
)