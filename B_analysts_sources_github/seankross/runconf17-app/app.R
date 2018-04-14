library(gh)
library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)
library(shiny)
library(plotly)

# You can create a GitHub PAT here: https://github.com/settings/tokens
Sys.setenv(GITHUB_PAT = "")

runconf17 <- gh("/repos/ropensci/unconf17/issues", .limit = Inf, .send_headers = c(Accept = "application/vnd.github.squirrel-girl-preview
"))

create_link <- function(url_, text_){
  paste0('<a target="_blank" href="', url_, '">', text_, '</a>')
}

runconf17_tbl <- map_df(runconf17, ~ tibble(
                           Title = create_link(.x$html_url, .x$title),
                           Title_raw = .x$title,
                           Labels = paste(map_chr(.x$labels, function(z){z$name}), collapse = ", "),
                           Number = as.numeric(.x$number), 
                           Reactions = .x$reactions$total_count,
                           Comments = .x$comments))

ui <- fluidPage(
  titlePanel("Browse runconf17 Projects"),
  verticalLayout(
    tabsetPanel(
      tabPanel("Table", dataTableOutput("tbl")),
      tabPanel("Plot", plotlyOutput("plotly_plot"))
    )
  )
)

server <- function(input, output){
  output$tbl <- renderDataTable({
    runconf17_tbl %>%
      select(-Title_raw)
  }, escape = FALSE)
  
  output$plotly_plot <- renderPlotly({
    plot_ly(runconf17_tbl, x = ~Reactions, y = ~Comments,
            type = 'scatter', mode = 'markers',
            text = ~paste(Title_raw, "<br>Number:", Number))
  })
}

shinyApp(ui = ui, server = server)