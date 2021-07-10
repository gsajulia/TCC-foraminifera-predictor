library(shinydashboard)
#(shinydashboardPlus)

my_theme <- create_theme(
  adminlte_color(
      aqua = "#FFC0CB", #Test
      light_blue = "#28A390"
  ),
  adminlte_sidebar(
    dark_bg = "#1F1F1F"
  )
)


# Put them together into a dashboardPage
ui <- dashboardPage(
      dashboardHeader(title = "Foraminifera"),
      dashboardSidebar(
        collapsed=TRUE,
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                   badgeLabel = "new", badgeColor = "red")
        )
      ),
      dashboardBody(
        use_theme(my_theme),
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