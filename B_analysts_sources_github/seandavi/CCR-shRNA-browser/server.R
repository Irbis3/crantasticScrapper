
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(dplyr)
library(yaml)

creds = yaml.load_file('creds.yaml')

db = src_mysql(dbname=creds$dbname,
               host=creds$host,
               password=creds$password,
               user=creds$user)
sgrna_mouse = tbl(db,"sgRNA_filtered_mouse")

data(iris)

library(shiny)

shinyServer(function(input, output) {
  
  gene = reactive({
    if(input$gene=='') return(data.frame())
    g = paste0('%',input$gene,'%')
    filter(sgrna_mouse,geneID %like% g) %>%
      collect()
  })
  output$text = renderText(paste0(input$gene,input$species))
  output$dataTable = DT::renderDataTable(
      gene(),
      rownames=FALSE,
      extensions=c('FixedHeader','ColVis'),
      options = list(dom = 'C<"clear">lfrtip')
    )
    
})
