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
          column(
            width = 4,
            graph_UI("graph_test"),

          ),
          title = "Predict",
          solidHeader = TRUE,
          column(width = 4,
          tags$div(`class` = "special-title", "Neural Network Model"),
          "Choose the parameter to generete the network",
          br(),
          "More box content",
          ),
          column(width = 4,
          tags$div(`class` = "special-title", "Neural Network Model"),
          "Choose the parameter to generete the network",
          br(),
          "More box content")
          
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