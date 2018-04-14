###############
# Create Guarantee Signalling Game Simulation Plots
# Christopher Gandrud
# 21 July 2014
# Depends on Python 2.7.6
# MIT License
###############

# Change working directory
setwd("/git_repositories/GuaranteeGame/")

# Load Packages
library(ggplot2)
library(gridExtra)

# Run game and simulate data
system('python GuaranteeGame2Signals.py')

# Import simulated data
Sims <- read.csv("SimulatedData/SimData09_2014.csv",
                 stringsAsFactors = FALSE)

# Convert strings to numeric
vars <- names(Sims)
for (i in vars){
    Sims[, i] <- as.numeric(Sims[, i])
}

# Create signaller combinations
Sims$Signalers[Sims$Signaler1 == -0.05 & Sims$Signaler2 == 0.05] <- "-0.05, 0.05"
Sims$Signalers[Sims$Signaler1 == -0.05 & Sims$Signaler2 == 0.15] <- "-0.05, 0.15"
Sims$Signalers[Sims$Signaler1 == -0.15 & Sims$Signaler2 == 0.05] <- "-0.15, 0.05"
Sims$Signalers[Sims$Signaler1 == -0.15 & Sims$Signaler2 == 0.15] <- "-0.15, 0.15"

#### Graph Utilities ####

# Save plot
pdf('~/Dropbox/Ireland_Korea_Research/Paper/Figures/TwoSignalers.pdf', height = 15)

# Guarantee - Alpha
GuarAlpha <- ggplot(Sims, aes(x = Alpha, y = Xreal)) +
                facet_grid(~Signalers) +
                geom_point() +
                scale_x_continuous(breaks = c(0.7, 0.8, 0.9),
                                    labels = c(0.7, 0.8, 0.9)) +
                xlab("") + ylab("Guarantee Decision - alpha i\n") +
                theme_bw(base_size = 10)

# Guarantee Decision (gk)
GuarPlot <- ggplot(Sims, aes(x = Alpha, y = Guarantee)) +
              facet_grid(~Signalers) +
              geom_point(color = "#E41A1C") +
              scale_x_continuous(breaks = c(0.7, 0.8, 0.9),
                                 labels = c(0.7, 0.8, 0.9)) +
              xlab("") + ylab("Guarantee Decision \n") +
              theme_bw(base_size = 10)

# PM's Utility
PMPlot <-ggplot(Sims, aes(x = Alpha, y = Upm)) +
            facet_grid(~Signalers) +
            geom_point(color = "#4DAF4A") +
            scale_x_continuous(breaks = c(0.7, 0.8, 0.9),
                               labels = c(0.7, 0.8, 0.9)) +
            xlab("") + ylab("Prime Minister's Utility \n") +
            theme_bw(base_size = 10)

# S1's Utility
S1Plot <- ggplot(Sims, aes(x = Alpha, y = Us1)) +
            facet_grid(~Signalers) +
            geom_point(color = "#1F78B4") +
            scale_x_continuous(breaks = c(0.7, 0.8, 0.9),
                               labels = c(0.7, 0.8, 0.9)) +
            xlab("") + ylab("MoF's Utility \n") +
            theme_bw(base_size = 10)

# S2's Utility
S2Plot <- ggplot(Sims, aes(x = Alpha, y = Us2)) +
            facet_grid(~Signalers) +
            geom_point(color = "#A6CEE3") +
            scale_x_continuous(breaks = c(0.7, 0.8, 0.9),
                               labels = c(0.7, 0.8, 0.9)) +
            xlab("\n alpha i") + ylab("FR's Utility \n") +
            theme_bw(base_size = 10)


# Combine the Graphs
grid.arrange(GuarAlpha, GuarPlot, PMPlot, S1Plot, S2Plot, nrow = 5)

dev.off()
