library(shinydashboard)

my_theme <- create_theme(
  adminlte_color(
      light_blue = "#28A390"
  ),
  adminlte_sidebar(
    dark_bg = "#212936"
  )
)


# Put them together into a dashboardPage
ui <- dashboardPage(
      dashboardHeader(title = "Foraminiferal"),
        dashboardSidebar(
          collapsed=FALSE,
          sidebarMenu(
            tags$div(`class` = "subtitle", "Predictions"),
            hr(),
            menuItem("Predict", tabName = "predict", icon = icon("searchengin")),
            menuItem("Neural Network Info", icon = icon("network-wired"), tabName = "neuralNetwork"),
            menuItem("Graphs", icon = icon("chart-bar"), tabName = "graphs"),
            hr(),
            menuItem("Help", icon = icon("question"), tabName = "help")
          )
        ),
        dashboardBody(
          use_theme(my_theme),
          tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
          # Including css
          tags$link(
            rel =  "stylesheet",
            type = "text/css",
            href = "styles.css"
          ),
          # Including fonts
          tags$link(
            rel =  "stylesheet",
            href="https://fonts.googleapis.com/css2?family=Qahiri&display=swap"
          ),
          tabItems(
            tabItem(tabName = "dashboard",
                    graph_UI("graph_test")
                    
            ),
            
            tabItem(tabName = "widgets",
                    h2("Widgets tab content")
            )
          )
        )
    )