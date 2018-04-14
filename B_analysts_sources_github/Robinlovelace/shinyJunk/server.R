# Define server logic for slider examples
shinyServer(function(input, output) {

  donations = readr::read_csv("data/TRJFP-all-intercepts.csv")

  df = reactive({

    donations = donations[donations$date > input$date[1] & donations$date < input$date[2],]
    # Compose data frame
    data.frame(
      Name = c("Bread",
               "Cheese"),
      Donations = c(sum(donations$weight[grep("Bread", donations$product)]),
                    sum(donations$weight[grep("Cheese", donations$product)]))
    )
  })

  # Show the values using an HTML table
  output$values <- renderPlot({
    barplot(height = df()$Donations, names.arg = df()$Name)
  })
})