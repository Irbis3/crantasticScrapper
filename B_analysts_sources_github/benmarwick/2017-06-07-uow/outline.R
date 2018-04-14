# 6th and 7th of June from 9 am to 1 pm.
# 2017-06-07-uow

# 9-10  Motivation for using R, Orientation to RStudio, Projects, folder organisation
# 10-11 R data types & object classes, create, access and subset with $ and [
# 11-12 Mathematic operations & plotting
# 12-1  exporting & saving table and plot output (CSV, PDF)


# 1) Basic types of R data (such as character, numeric, factor, etc) and the structure of R objects (such as singular, vector, matrix, dataframe, class). 

#- vector
#-- numeric, character, logical, factor
x0 <- c(5, 6)
x2 <- c(1, 2, 3)
x3 <- c("BA", "MSc", "PhD")
x4 <- c("TRUE", "FALSE", "TRUE")
x3[3]
x2 > 2

#- data frame
#-- tabular structure, can take different col classes
#-- list of variables of same number of rows
y <- data.frame(x = 1:10, y = 10:1)
y[1, 1:2]
y['y']

#- matrix
#--  tabular structure, all col must be the same class
z <- matrix(1:15, nrow = 5, ncol = 3)

# list
#-- a kind of vector that can hold anything
a <- list(x, x1, y, z)
a[1]
a[c(1, 4)]
a[[1]]

# 2) Basic manipulation on the R objects, including how to create, access and subset different elements and attributes of the objects (such as the $ operator) 

x1more <- c(x1, "DSc")
x1[3] <- "Cert"    
x1[5] <- "Prof"
y$newcol <- c(rep("a", 5), rep("b", 5))

# 3) Simple mathematic operation between R objects. 

#- + - * / 
#- vectorised: x0[1] + y['x'] 
#- mean, sd, don't forget na.rm = TRUE


# 4) If time is allowed, some introduction on simple plotting is good, such as plotting scatter (x-y) plots (which is the most used in OSL dating) using either the plot() function or the ggplot. 

library(readr)
ms <- read_csv("data/D_0_1919.csv")

# set out the plot frame, specify the dataframe and columns to plot
library(tidyverse)
ggplot(ms, 
       aes(D0, 
           De)) +
  # add points to make a scatter plot
  geom_point() 


# add error bars
ggplot(ms, 
       aes(D0, 
           De)) +
  # add points to make a scatter plot
  geom_point() +
  # add vertical error bars
  geom_errorbar(aes(ymin = De - DeErr, 
                    ymax = De + DeErr,
                    width = 0)) +
  # add horzontal error bars
  geom_errorbarh(aes(xmin = D0 - D0_Err, 
                     xmax = D0 + D0_Err, 
                     height = 0)) 



# add title and style
ggplot(ms, 
       aes(D0, 
           De)) +
  # add points to make a scatter plot
  geom_point() +
  # add vertical error bars
  geom_errorbar(aes(ymin = De - DeErr, 
                    ymax = De + DeErr,
                    width = 0)) +
  # add horzontal error bars
  geom_errorbarh(aes(xmin = D0 - D0_Err, 
                     xmax = D0 + D0_Err, 
                     height = 0)) +
  theme_bw() +
  coord_equal() +
  ggtitle("BDL2-OSL 3/1 - UOW 1919 (80cm)")

# use colour or shape of points
ms$dummy <- ifelse(ms$De > 100, "greater", "less")


my_plot <- 
ggplot(ms, 
       aes(D0, 
           De,
           colour = dummy,
           label = b_value)) + # for ggplotly
  # add points to make a scatter plot
  geom_point() +
  # add vertical error bars
  geom_errorbar(aes(ymin = De - DeErr, 
                    ymax = De + DeErr,
                    width = 0)) +
  # add horzontal error bars
  geom_errorbarh(aes(xmin = D0 - D0_Err, 
                     xmax = D0 + D0_Err, 
                     height = 0)) +
  theme_bw() +
  coord_equal() +
  ggtitle("BDL2-OSL 3/1 - UOW 1919 (80cm)") +
  # note the manual colour here:
  scale_colour_brewer(palette = "Dark2")

# interactive is useful
library(plotly)
ggplotly(my_plot)


# 5) How to save the output of R (such as write.cvs and plot pdf) 

# never use save() or save.image()

write.csv(y)
write.csv(y, row.names = FALSE)

my_plot
ggsave("my_plot.png")# not good location

ggsave("output/my_plot.png") # better

ggsave("output/my_plot.png", # more control over size
       width = 100, 
       height = 60, 
       units = "mm")

#- base plot
png("output/my_plot.png")
plot(1:10)
dev.off()

pdf("output/my_plot.pdf")
plot(1:10)
dev.off()





library(numOSL)
joul17 <- loadBINdata("data/JOUL17-G9_101(SG)discs31-39.BIN",
                      view = FALSE)

str(joul17)

# visualize decay curves for Position 31

obj_pickBIN <- pickBINdata(joul17, 
                           Position=31, 
                           view=FALSE,
                           LType="OSL", 
                           force.matrix=TRUE)

obj_pickBIN[[1]][1]

matplot(obj_pickBIN$BINdata[[1]][,1], 
        obj_pickBIN$BINdata[[1]][,-1], 
        main="Decay curves",
        xlab="Stimulation time (s)",
        ylab="Photon counts",
        type="l", log="xy")

library(dplyr)
library(tidyr)
decay_curves_data <- 
obj_pickBIN$BINdata[[1]] %>% 
  as_data_frame() %>% 
  gather(grain, 
         photon_count , 
         -x) %>% 
  rename(simulation_time = x ) %>% 
  mutate(photon_count = photon_count + 0.01)
  

library(ggplot2)
library(scales)
decay_curves_plot <- 
ggplot(decay_curves_data,
       aes(simulation_time,
           photon_count,
           colour = grain)) +
  geom_line() +
  scale_y_continuous(trans = "log2") +
  scale_x_continuous(trans = "log2") +
  theme_bw() +
  ggtitle("Decay curves") +
  theme(legend.position="none") +
  labs(x = "Simulation time (s)", 
       y = "Photon count")

library(plotly)
ggplotly(decay_curves_plot)


### Example 3 (visualize regenerative-dose decay curves for Position=2):
data(BIN)
obj_pickBIN <- pickBINdata(BIN, Position=2, Run=c(8,14,20,26,31,37), 
                           view=FALSE, LType="OSL", force.matrix=TRUE)


matplot(obj_pickBIN$BINdata[[1]][,1], 
        obj_pickBIN$BINdata[[1]][,-1], 
        main="Regenerative-dose decay curves",
        xlab="Stimulation time (s)",
        ylab="Photon counts",
        type="l", log="xy")

library(dplyr)
library(tidyr)
decay_curves_data <- 
  obj_pickBIN$BINdata[[1]] %>% 
  as_data_frame() %>% 
  gather(grain, 
         photon_count , 
         -x) %>% 
  rename(simulation_time = x ) %>% 
  mutate(photon_count = photon_count + 0.01)

library(ggplot2)
library(scales)
regenerative_dose_decay_curves_plot <- 
  ggplot(decay_curves_data,
         aes(simulation_time,
             photon_count,
             colour = grain)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = "log2") +
  scale_x_continuous(trans = "log2") +
  theme_bw() +
  ggtitle("Regenerative-dose decay curves") +
  theme(legend.position="none") +
  labs(x = "Simulation time (s)", 
       y = "Photon count")


# read in the data
library(tidyverse)
ms <- read_csv("data/D_0_1919.csv")

# set out the plot frame, specify the dataframe and columns to plot
ggplot(ms, 
       aes(D0, 
           De)) +
  # add points to make a scatter plot
  geom_point() +
  # add vertical error bars
  geom_errorbar(aes(ymin = De - DeErr, 
                    ymax = De + DeErr,
                    width = 0)) +
  # add horzontal error bars
  geom_errorbarh(aes(xmin = D0 - D0_Err, 
                     xmax = D0 + D0_Err, 
                     height = 0)) +
  theme_bw() +
  ggtitle("BDL2-OSL 3/1 - UOW 1919 (80cm)")
       
       
