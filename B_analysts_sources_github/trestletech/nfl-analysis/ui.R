punters <- c("M. Allen","B. Anger","L. Araguz","J. Baker","B. Barker","B. Barnard","M. Barr","D. Bennett","M. Berger","J. Bidwell","M. Bosher","D. Brooks","C. Bryan","D. Butler","S. Cheek","B. Colquitt","D. Colquitt","T. Conley","P. Dawson","M. Dodge","R. Donahue","P. Ernster","J. Feagles","B. Fields","F. Filipovic","D. Frost","C. Gardocki","T. Gowin","B. Graham","A. Groom","C. Hanson","N. Harris","B. Hartmann","J. Hekker","C. Henry","C. Hentrich","R. Hodges","K. Huber","C. Jarrett","J. Jett","D. Johnson","E. Johnson","L. Johnson","C. Jones","D. Jones","J. Kapinos","B. Kern","C. Kluwe","M. Knorr","S. Koch","M. Koenen","B. LaFleur","S. Landeta","K. Larson","S. Lechler","A. Lee","R. Malone","T. Masthay","B. Maynard","P. McAfee","M. McBriar","Z. Mesko","J. Miller","C. Mohr","B. Moorman","T. Morstead","N. Murphy","B. Nortman","G. Pakulak","S. Paulescu","R. Plackemeier","S. Player","A. Podlesh","S. Powell","K. Richardson","S. Rocca","T. Rouen","M. Royals","J. Ryan","B. Sander","T. Sauerbrun","M. Scifres","D. Sepulveda","H. Smith","C. Stanley","K. Stemke","D. Stryzinski","T. Tupa","M. Turk","K. Walter","S. Weatherford","D. Zastudil")

shinyUI(pageWithSidebar(
  headerPanel("NFL Punting Data"),
  sidebarPanel(selectInput(inputId = "punter",
                            label = "Select the punter's name:",
                            choices = c("All", punters)),
               conditionalPanel(condition="punter.value != 'All'", 
                                tableOutput("data")                                
                                ),
               plotOutput("resultsBar", height="200px"),
               plotOutput("years", height="200px")
               ),
  mainPanel(
            plotOutput(outputId = "main_plot", height = "600px"))
  
))