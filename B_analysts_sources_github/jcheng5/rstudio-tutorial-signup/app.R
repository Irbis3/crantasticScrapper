library(shiny)

ui <- fixedPage(fixedRow(
  column(8, offset = 2,
    h3("Welcome to the Effective Shiny tutorial!"),
    p("We've prepared RStudio Server accounts for this class, which ",
      "you'll need to use to complete the exercises. Fill out the ",
      "form below to set up an account."
    ),
    br(),
    conditionalPanel("!input.register",
      textInput("email", NULL, placeholder = "Email address"),
      actionButton("register", "Continue", class = "btn-primary")
    ),
    conditionalPanel("input.register",
      uiOutput("credentials"),
      br(),
      downloadButton("download", "Download these credentials"),
      br(),
      br(),
      a(href = "/", target = "_blank",
        "Sign in to RStudio Server"
      )
    )
  )
))

server <- function(input, output, session) {
  rv <- reactiveValues(id = NULL)
  observeEvent(input$register, {
    id <- readBin("last.row", "integer") + 1L
    writeBin(id, "last.row")
    rv$id <- id
  })
  
  observeEvent(credentials(), {
    cat(file = "signups.log", credentials()$user, input$email, "\n", append = TRUE)
  })
  
  credentials <- reactive({
    req(rv$id)
    
    df <- read.csv("passwords.txt", col.names = c("user", "pass"),
      stringsAsFactors = FALSE, header = FALSE)
    
    as.list(df[rv$id,])
  })
  
  output$credentials <- renderUI({
    tagList(
      div(strong("Username: "), credentials()$user),
      div(strong("Password: "), credentials()$pass)
    )
  })
  
  output$download <- downloadHandler(
    "shiny_tutorial_creds.txt",
    function(file) {
      writeLines(
        sprintf("Username: %s\nPassword: %s\n",
          credentials()$user,
          credentials()$pass
        ),
        file
      )
    }
  )
}

shinyApp(ui, server)
