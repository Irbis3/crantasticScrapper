setwd("~/Desktop/Online Coursera/Coursera-Exploratory-Data-Analysis/ExData_Plotting2/")
unzip("./data/exdata-data-NEI_data.zip", exdir = "./data/")
# Check if both data exist in the environment. If not, load the data.
if (!"neiData" %in% ls()) {
    neiData <- readRDS("./data/summarySCC_PM25.rds")
}
if (!"sccData" %in% ls()) {
    sccData <- readRDS("./data/Source_Classification_Code.rds")
}

head(sccData$Short.Name)

# if (!"load_data.R" %in% list.files()) {
#     setwd("~/Desktop/Online Coursera/Coursera-Exploratory-Data-Analysis/ExData_Plotting2/")
# } 
# source("load_data.R")

par("mar"=c(5.1, 4.5, 4.1, 2.1))
png(filename = "./figure/plot4.png", 
    width = 480, height = 480, 
    units = "px")
coal <- grep("coal", sccData$Short.Name, ignore.case = T)
coal <- sccData[coal, ]
coal <- neiData[neiData$SCC %in% coal$SCC, ]

coalEmissions <- aggregate(coal$Emissions, list(coal$year), FUN = "sum")
# options(scipen=0)
# options(scipen=999)
plot(coalEmissions, type = "l", xlab = "Year", 
     main = "Total Emissions From Coal Combustion-related\n Sources from 1999 to 2008", 
     ylab = expression('Total PM'[2.5]*" Emission"))

dev.off()

# ### Another understanding (smaller difference)
# # Across the United States, how have emissions from coal combustion-related sources 
# # changed from 1999–2008?
# 
# # Utilize plyr to split-apply-combine to simplify our dataset
# library(plyr)
# # Utilizing ggplot2 for this plot
# library(ggplot2)
# 
# unzip("./data/exdata-data-NEI_data.zip", exdir = "./data/")
# 
# #Read in the data file from disk. These are RDS files
# SCC <- readRDS("./data/Source_Classification_Code.rds")
# NEI <- readRDS("./data/summarySCC_PM25.rds")
# 
# #Utilize only the Coal Combustion sources as found via the patter of Comb and Coal in the short name
# #Collect the SCC numbers for the coal combustion sources
# coalcom <- SCC[grep("Comb.*Coal", SCC$Short.Name), "SCC"]
# 
# #Create new dataframe of just the coalcombustion sources
# coalcombNEI <- NEI[NEI$SCC %in% coalcom, ]
# 
# # Summarize the emissions by year to simplify the plot
# total <- ddply(coalcombNEI, .(year), 
#                      summarise, 
#                      TotalEmissions = sum(Emissions))
# 
# # Set the graphics device to png
# png(filename = "./figure/plot42.png")
# 
# # Plot the data -- finding total emissions each year
# ggplot(total, aes(year, TotalEmissions)) +
#     geom_line() + geom_point() +
#     labs(title = "Total Emissions from Coal Combustion-Related Sources",
#          x = "Year", y = "Total Emissions")
# 
# dev.off()