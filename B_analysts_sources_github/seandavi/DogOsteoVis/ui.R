datasets = c(CSU='GSE76127',NCI='GSE16087')

shinyUI(fluidPage(
    titlePanel('Canine Osteosarcoma Gene Lookup'),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            selectInput("dataset",choices=names(datasets),label='Choose a dataset'),
            textInput("symbol",
                      label="Choose a gene to plot",
                      value='ACTA1'),
            submitButton('Replot')
        ),
        mainPanel = mainPanel(
            plotOutput('bplot')
        )
    )
)
)
