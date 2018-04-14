library(GEOquery)
library(ggplot2)
library(tidyr)
datasets = c(CSU='GSE76127',NCI='GSE16087')
gselist = lapply(datasets,function(x) {
    return(getGEO(x)[[1]])})
names(gselist)=names(datasets)

shinyServer(function(input,output,session) {
    output$message <- renderText({
        paste("Input text is:", input$symbol)
    })

    df = reactive({
        gse = gselist[[input$dataset]]
        validate(
            need(input$symbol %in% fData(gse)$'Gene Symbol', "Gene not found in this dataset")
        )
        if(!(input$symbol %in% fData(gse)$'Gene Symbol')) {
            output$message = "Gene not found in dataset"
        }
        idx = fData(gse)$'Gene Symbol'==input$symbol
        mat = subset(exprs(gse),idx)
        colnames(mat) = pData(gse)$title
        return(data.frame(Probe=featureNames(gse)[idx],mat) %>%
               gather(Sample,Expression,-Probe))
    })
        
    
    output$bplot = renderPlot(
        ggplot(df(),aes(y=Expression,x=Sample)) + geom_point(aes(shape=Probe,color=Probe)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    )
})
