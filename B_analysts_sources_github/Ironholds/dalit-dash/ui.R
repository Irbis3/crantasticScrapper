library(shinydashboard)
library(dygraphs)
library(shiny)
library(markdown)

dashboardPage(
  dashboardHeader(title = "Dalit History Month"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Article Count", tabName = "article_count"),
      menuItem("Article Quality", tabName = "article_quality")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "article_count",
              dygraphOutput("count_graph"),
              includeMarkdown("count.md")
      ),
      
      # Second tab content
      tabItem(tabName = "article_quality",
              dygraphOutput("class_graph"),
              includeMarkdown("quality.md")
      )
    )
  ), skin = "black"
)

