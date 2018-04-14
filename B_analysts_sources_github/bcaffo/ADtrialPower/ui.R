# Define UI for simulating power for a sample size calculation
shinyUI(
    pageWithSidebar(  
        # Application title
        headerPanel("Power calculations for an AD trial"),
        sidebarPanel(
            h4("Enter simulation settings, the defaults are estimated from our analysis of ADNI"),
            numericInput("beta1", "Yearly increase in CDR SB for diseased group", 0.924, min = 0, max = 2, step = .001),
            numericInput("perd", "Percent decrease in the incline in CDR SB for treated group expressed as a proportion (example to consider a 30% decrease, going from a 1.0 mean yearly increase in CDR SB to 0.7, enter 0.30)", 0.30, min = 0.01, max = 1.0, step = .01 ),
            numericInput("sigma0", "Random intercept standard deviation (remember to express as sd)", 0.762, min = 0, max = 2, step = .01),
            numericInput("sigma1", "Random slope standard deviation (remember to express as a sd)", 0.844, min = 0, max = 2, step = .01),
            numericInput("cor12", "Correlation between RI and RS", 0.297, min = -1, max = 1, step = .1),
            numericInput("sigma", "Error standard deviation (remember to express as a sd)", 0.628, min = 0.1, max = 2, step = .1),
            selectInput("method", "Simulated and fitted model:",
                        list("Random intercept and slope" = "rirs", 
                             "Random slope only" = "rs", 
                             "Random intercept only" = "ri")),
            numericInput("n", "Number of subject per arm", 100, min = 10, step = 10),
            numericInput("arms", "Number of treatment arms, (excluding the control)", 1, min = 1, step =1),
            numericInput("m", "Number of equally spaced visits, 2 for only baseline and followup", 3, min = 2, step = 1),
            numericInput("yrs", "Total length of trial in years", 1, min = .5, step = .5),
            numericInput("nosim", "Number of simulations to run. Use 10 to test things out, 100 for preliminary results and at least 1,000 for final results", 10, min = 100, step = 100),
            numericInput("alpha", "Type 1 error rate. Probably leave this at .05. Simulation further assumes two sided test, corrected for all comparisons against the control group. So 3 comparisons for a 3 armed trial.", .05, min = .01, max = .99, step = .01)
        ),
        mainPanel(
            actionButton("go", "Click here to run the simulation"),
            h3('Results of simulation'),
            verbatimTextOutput("power"),
            h3('Under the assumptions below.'),
            h4('Your trial has visit times in years'),
            verbatimTextOutput("mtime"),
            h4('Control slope, treated slope, delta slope, percent decrease'),
            verbatimTextOutput("slopes"),         
            h4('Random intercept sd, random slope sd, cor between RI/RS, error sd'),
            verbatimTextOutput("varcomps"),
            h4('Method used for simulation and estimation'),
            verbatimTextOutput("method"),
            h4('Type 1 error'),
            verbatimTextOutput("alpha")
          )
    )
)