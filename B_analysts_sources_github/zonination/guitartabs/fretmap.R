# Load libraries
# library(tidyverse)
# setwd("C:/path/to/folder") # Switch all "\" to "/"

# Load lookup tables, set up a list
tablist<-list.files("tabs", pattern="csv")    #lists the tabs/ folder
tablist<-substr(tablist, 1, nchar(tablist)-4) #remove teh ".csv"

# Set initial List of Data Frames
eval(parse(text=paste("tabs<-list(",
           tablist[1],
           "=read_csv('tabs/",
           tablist[1],
           ".csv'))",
           sep="")))

# Collect all tabs with a For loop
for(n in 2:length(tablist)){
  eval(parse(text=paste("tabs$",
                        tablist[n],
                        "<-read_csv('tabs/",
                        tablist[n],
                        ".csv')",
                        sep="")))
}; rm(n); rm(tablist)