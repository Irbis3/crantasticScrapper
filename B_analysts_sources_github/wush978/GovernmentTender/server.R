library(shiny)
library(data.table)
library(dplyr)
library(igraph)

tenders <- readRDS("data/tenders.Rds")


tenderers <- readRDS("data/Tenderer.Rds") %>%
  filter(!duplicated(廠商代碼)) %>%
  data.table
setkey(tenderers, "廠商代碼")

graphs <- c(公司地址="graph/Graph_addr", 公司董監事="graph/Graph_dupboss", 投資關係="graph/Graph_comivst")

srclist <- lapply(graphs, function(path) {
  fread(path, sep = "\t", header = FALSE, showProgress = interactive()) %>%
  `colnames<-`(c("n1", "n2", "weight"))
})
names(srclist) <- graphs

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  query <- reactive({
    tmp <- filter(tenders, pkAtmMain == input$pkAtmMain)
    idlist <- unique(tmp[["廠商代碼"]])
    idlist <- paste(idlist, collapse=",")
    tmp.path <- tempfile(fileext = ".csv")
    out.path <- tempfile(fileext = ".csv")
    tryCatch({
      src <- srclist[[input$graph]]
      threshold <- quantile(src$weight, input$threshold / 100, na.rm = TRUE)
      filter(src, weight >= threshold) %>%
        select(n1, n2) %>%
        write.table(tmp.path, row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
#       system(sprintf("python/debug.py %s %s %s", tmp.path, idlist, out.path))  
      min_len <- system(sprintf("python/query.py %s %s %s", tmp.path, idlist, out.path), intern = TRUE)  
      list(min_len = as.numeric(min_len), subgraph = {
        readLines(out.path) %>%
             strsplit("\t")
        }, idlist = idlist)
    }, finally = {
      file.remove(tmp.path)
      file.remove(out.path)
    })  
  })
  
  output$tendererData <- renderDataTable({
    result <- query()
    v <- strsplit(result$idlist, split=",")[[1]]
    if (length(v) == 0) {
      data.frame(結果="查無資料")
    } else {
      tenderers[J(v)] %>%
        select(廠商代碼, 廠商名稱, 組織型態, 是否為中小企業, 雇用員工總人數是否超過100人, 廠商地址)      
    }
  })
  
  output$tendererRelatedData <- renderDataTable({
    result <- query()
    v <- unique(unlist(result$subgraph))
    if (length(v) == 0) {
      data.frame(結果="查無資料")
    } else {
      tenderers[J(v)] %>%
        select(廠商代碼, 廠商名稱, 組織型態, 是否為中小企業, 雇用員工總人數是否超過100人, 廠商地址)      
    }
  })
  
  output$neighborPlot <- renderPlot({
    result <- query()
#     browser()
    if (length(result$subgraph) == 0) {
      
    } else {
      src <- lapply(result$subgraph, function(x) tenderers[J(x)][["廠商名稱"]])
      v <- unlist(src) %>% unique
      e <- lapply(src, function(x) edge(x))
      g <- graph.empty(directed = FALSE) + vertices(v)
      for(i in seq_along(e)) {
        g <- g + e[[i]]
      }
      plot(g)
    }
  })
})