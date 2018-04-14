
# collab text area: https://public.etherpad-mozilla.org/p/Apr-2017-coda-R-intro (Please enter your name)

# assume we have R, RStudio installed

# this tutorial is based on 
# - http://www.datacarpentry.org/R-ecology-lesson/
# - http://swcarpentry.github.io/r-novice-gapminder/
# both of these are much more detailed and ideal for self-study, highly recommended!
# for further self-study, I highly recommend http://swirlstats.com/

# Interacting with R & RStudio --------------------
# ---------------------------------------------------


# - console, script editor, plots, environment
# - assigning values to objects, using objects

2 + 3
x <- 2
y <- 3
x + y
z <- x + y

# if you get stuck with a "+" symbol instead of a ">" symbol,
# e.g., type this in console and press enter: 2 + 3 + 
# then you just need to press ESC (windows) or CTRL + C (Mac) 
# to interrupt and get back to the normal prompt. 

# - commenting code
# - using functions

x <- c(4, 7, 12) # length measurements of three artefacts
# this is a vector! a sequence of elements of same type
x

y <- mean(x)     # compute the mean length
?mean            # get help on a specific function
??median         # search help docs

# - packages

# all the best pkgs for archaeology are listed here:
# https://github.com/benmarwick/ctv-archaeology

# how to install, from the R prompt:

install.packages("readr") # wait
install.packages("readxl")

# - getting help: 
# -- searching google: "r help ..."
# -- SO, email lists, great advice: http://jennybc.github.io/reprex/
# -- self-teaching: swirl, https://www.rstudio.com/resources/cheatsheets/
  
# Importing tabular data -----------------------------
#  ---------------------------------------------------

#- working directory RStudio Projects
#-- we set with Session -> Set Working Directory -> To source file location

#- Import CSV and Excel files, tab-complete!

library(readr)
ktc_ceramic_data <- read_csv("data/ktc_ceramic_data.csv")
# this is a data frame! basic tabular structure
View(ktc_ceramic_data)

library(readxl)
jerimalai_lithics <- read_excel("data/jerimalai_lithics.xlsx")
# output is data frame

#-  multiple files

# just two steps:

# create a list of files that we want to import
my_list_of_files <- list.files(path = "data/many_excel_files/",
                               full.names = TRUE)
# this is a character vector!

library(purrr)
data_from_my_files <- map(my_list_of_files,
                          ~read_excel(.x))
# this is a list!

# Inspecting data ------------------------------------
#  ---------------------------------------------------

# - basic functions for seeing what we have
  
# show structure
str(y) # vector
str(ktc_ceramic_data)      # data frame
## do CTRL + L to clear console ##

# a nicer look a structure for data frames
library(dplyr)
glimpse()

# look at the first and last parts
head(ktc_ceramic_data)
tail(ktc_ceramic_data)

# get column names
names(ktc_ceramic_data)

# see as table in RStudio, can sort (by EU) and filter (for CC) easily
View(ktc_ceramic_data)

# - indexing with [ , ] and $

# to get a single column from a data frame, use $

j_weights <- jerimalai_lithics$Weight
mean(j_weights) # NA , why?
?mean           # check help documentation
mean(j_weights, na.rm = TRUE) # ok!

# to subset certain rows or columns from a data frame, use [ row , column ]

# we can use numbers
jerimalai_lithics[ 1:10, ]     # rows 1-10,  all cols
jerimalai_lithics[ 1:10, 1:4]  # rows 1-10, 1-4 cols only

# or names
# only rows with 'Chert' as raw material
jerimalai_lithics[ jerimalai_lithics$Material == "Chert", ]
# what does it do by itself?
jerimalai_lithics$Material == "Chert" # it's a logical vector!


# Exploratory Data Analysis: 5 main verbs ------------
#  ---------------------------------------------------

# - 5MV of dplyr: filter, mutate, arrange, summarise, group_by 

library(dplyr) # more info: https://cran.r-project.org/web/packages/dplyr/vignettes/introduction.html

## Five Main Verbs: filter, mutate, arrange, summarise, group_by  ##

## 1. Filter ##

# filter to get only rows that match a condition
# equal to (equivalent to)
jerimalai_lithics %>% 
  filter(Material == "Chert")

# filter to get only rows that match conditions
# numeric greater/less than
jerimalai_lithics %>% 
  filter(Material == "Chert" &
           Weight > 0.5)

# filter to get only rows that match conditions
# numeric range of values, we can use >, <=, >=, ==, and !=
jerimalai_lithics %>% 
  filter(Material == "Chert" &
         Weight >= 0.5 & 
         Weight <= 5)

# filter to get only rows that match conditions
# match multiple conditions

# what materials do we have?
unique(jerimalai_lithics$Material) # the unique values
table(jerimalai_lithics$Material)  # each value appears in how many rows?

# use %in% to match with multiple stone types
jerimalai_lithics %>% 
  filter(Material %in% c("Chert", "Volcanic") &
           Weight > 0.5 & 
           Weight < 5)

# challenge: in the ktc_ceramic_data, find rows with count >5 pieces
ktc_ceramic_data %>% 
  filter(Counts > 5)

## 2. Mutate ##

jerimalai_lithics %>% 
  mutate(flake_area = Length * Width)

# challenge: in the ktc_ceramic_data, create a new column with the average fragment mass (Mass / Counts)
ktc_ceramic_data %>% 
  mutate(av_mass = Counts / Mass)

## 3. Arrange ##

# sort ascending
jerimalai_lithics %>% 
  arrange(Weight)

# sort descending
jerimalai_lithics %>% 
  arrange(desc(Weight))

# challenge: in the ktc_ceramic_data, arrange table by Mass, highest to lowest
ktc_ceramic_data %>% 
  arrange(desc(Mass))

## 4. Summarise ##

jerimalai_lithics %>% 
  summarise(mean_weight = mean(Weight)) # why NA?

jerimalai_lithics %>% 
  summarise(mean_weight = mean(Weight, na.rm = TRUE))

# how about mean weight for all artefacts of one raw material?
jerimalai_lithics %>% 
  filter(Material == "Chert") %>% 
  summarise(mean_weight = mean(Weight, na.rm = TRUE))
# what about for all raw materials? We need to use group_by...

# challenge: in the ktc_ceramic_data, find the total mass of ceramics in this dig
ktc_ceramic_data %>% 
  summarise(total_mass = sum(Mass))

## 5. Group by ##

# get counts of artefacts for each raw material
jerimalai_lithics %>% 
  group_by(Material)  %>% 
  tally() 

# arrange to see most abundant...
jerimalai_lithics %>% 
  group_by(Material)  %>% 
  tally() %>% 
  arrange(desc(n))

# use mutate to add column of percentages...
jerimalai_lithics_raw_materials <- 
jerimalai_lithics %>% 
  group_by(Material)  %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(perc = n / sum(n) * 100)

# use round to  trim off unwanted decimal places...
jerimalai_lithics %>% 
  group_by(Material) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(perc = round(n / sum(n) * 100, 2))

# now back to the mean weights... here's what we can do:
jerimalai_lithics %>% 
  group_by(Material) %>% 
  summarise(mean_weight = mean(Weight, na.rm = TRUE),
            mean_length = mean(Length, na.rm = TRUE))

# compute means for all numeric columns!! incredibly efficient
jerimalai_lithics %>% 
  group_by(Material) %>% 
  summarise_if(is.numeric, 
               mean, 
               na.rm = TRUE)

# compute mean and standard devations for all numeric cols!!! incredible!
jerimalai_lithics %>% 
  group_by(Material) %>% 
  summarise_if(is.numeric, 
               funs(mean, sd), 
               na.rm = TRUE)

# challenge: in the ktc_ceramic_data, find the averag mass of ceramics in each Excavation Unit
ktc_ceramic_data %>% 
  group_by(EU) %>% 
  summarise(av_mass = mean(Mass))

# see the cheatsheets for more amazing things you can do with dplyr!
# also lots of nice tutorials online be researchers documenting their use of dplyr

# Exploratory Data Analysis: 5 named plots -----------
#  ---------------------------------------------------

## Exploratory Data Analysis: plotting & interactivity
# - 5NP of ggplot2: histograms, bar plot, line plot, boxplots, scatter-plots 


# how to choose which plot for your data?
# - http://extremepresentation.typepad.com/blog/2006/09/choosing_a_good.html
# further reading:
# -  "Creating More Effective Graphs" by Naomi Robbins. 

library(ggplot2) # good documentation at http://docs.ggplot2.org/current/ & http://www.cookbook-r.com/Graphs/

## 1. histogram ##

# good for showing the distribution of one variable

ggplot(jerimalai_lithics, 
       aes(Length)) +
  geom_histogram()

# we can edit 
#- bins = 10, 100 for hist

# for plots more generally:
#- axis labels, 
#- theme and font size
#- colour and fill

# challenge: in the ktc_ceramic_data, plot hist of mass
ggplot(ktc_ceramic_data, 
       aes(Mass)) +
  geom_histogram()


## 2. bar plot ##

# good for COUNTS ONLY of items in different categories
# DO NOT use for mean/median/etc distribution summaries

ggplot(jerimalai_lithics_raw_materials, 
       aes(Material,
           perc)) +
  geom_col()

# let's order the columns big to small
ggplot(jerimalai_lithics_raw_materials, 
       aes(reorder(Material, -perc),
           perc)) +
  geom_col()

# deal with long names on x-axis ticks
# flip plot
ggplot(jerimalai_lithics_raw_materials, 
       aes(reorder(Material, perc),
           perc)) +
  geom_col() +
  coord_flip()

# remove NA & plot
jerimalai_lithics_raw_materials %>% 
  filter(Material != "NA") %>% 
  ggplot(aes(reorder(Material, perc),
             perc)) +
  geom_col() +
  coord_flip()

# deal with long names on x-axis ticks
# rotate tick lables
ggplot(jerimalai_lithics_raw_materials, 
       aes(reorder(Material, perc),
           perc)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

# customise axis labels
ggplot(jerimalai_lithics_raw_materials, 
       aes(reorder(Material, perc),
           perc)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5)) +
  xlab("Raw material type") +
  ylab("Percentage")

# modify the theme, overall look, increase font size
ggplot(jerimalai_lithics_raw_materials, 
       aes(reorder(Material, perc),
           perc)) +
  geom_col() +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5)) +
  xlab("Raw material type") +
  ylab("Percentage") 
# there are so many wonderful themes! Technical details: http://docs.ggplot2.org/dev/vignettes/themes.html
# add-on themes: https://www.ggplot2-exts.org/index.html, e.g. https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html


# challenge: in the ktc_ceramic_data, plot counts per EU
ggplot(ktc_ceramic_data, 
       aes(EU, Counts)) +
  geom_col()


## 3. line plot ## 

# good to show change over a continunous variable, such as time. 
# DO NOT use for discrete categories (types, locations, etc)

j_spit_length <- 
  jerimalai_lithics %>% 
  group_by(Spit) %>%
  summarise(mean_length = mean(Length, 
                               na.rm = TRUE))

ggplot(j_spit_length,
       aes(Spit,
           mean_length)) +
  geom_line() +
  theme_bw(base_size = 14)


# remember that line graphs MUST have continuous variable for x-axis!!

## 4. boxplot ##

# Best choice for showing distributions in different categories
  
# we have do some work to make this look good!
ggplot(jerimalai_lithics,
       aes(Material,
           Weight)) +
  geom_boxplot()

# there is a NA category, let's get rid of that
jerimalai_lithics_to_plot <- 
  jerimalai_lithics %>% 
  filter(Material != "NA")

# check it out...

ggplot(jerimalai_lithics_to_plot,
       aes(Material,
           Weight)) +
  geom_boxplot()

# no NA now

# use log scale for y-axis
ggplot(jerimalai_lithics_to_plot,
       aes(Material,
           Weight)) +
  geom_boxplot() +
  scale_y_log10()

# rotate x-axis labels
ggplot(jerimalai_lithics_to_plot,
       aes(Material,
           Weight)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4))

# put categories in order, no, this has no effect!
ggplot(jerimalai_lithics_to_plot,
       aes(reorder(Material, -Weight),
           Weight)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4))

# problem because we have a few artefacts where Weight = 0, this is
# obviously a mistake

# let's quickly see how many
sum(jerimalai_lithics_to_plot$Weight == 0 , na.rm = TRUE)
# not many, let's exclude them

jerimalai_lithics_to_plot <- 
  jerimalai_lithics %>% 
  filter(Weight != 0)

# try again, wihtout 0s, that looks a bit better
ggplot(jerimalai_lithics_to_plot,
       aes(reorder(Material, -Weight),
           Weight)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4))

# fix the axis titles
ggplot(jerimalai_lithics_to_plot,
       aes(reorder(Material, -Weight),
           Weight)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4)) +
  xlab("Raw Material") +
  ylab("Weight (g)")

# fix the y-axis tick labels
library(scales)
ggplot(jerimalai_lithics_to_plot,
       aes(reorder(Material, -Weight),
           Weight)) +
  geom_boxplot() +
  scale_y_log10(labels = comma) +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4)) +
  xlab("Raw Material") +
  ylab("Weight (g)")

# use nice theme
ggplot(jerimalai_lithics_to_plot,
       aes(reorder(Material, -Weight),
           Weight)) +
  geom_boxplot() +
  scale_y_log10(labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4)) +
  xlab("Raw Material")  +
  ylab("Weight (g)")

# this is excellent, suitable for publication, and better than many we see in papers
# one further improvement we can make is to show the data for each individual artefact

library(ggforce)
ggplot(jerimalai_lithics_to_plot,
       aes(reorder(Material, -Weight),
           Weight)) +
  geom_boxplot() +
  geom_sina(alpha = 0.005) + # experiment with the value for alpha
  scale_y_log10(labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4)) +
  xlab("Raw Material")  +
  ylab("Weight (g)")

# Gives more information about the data - which category has the most artefacts, 
# what the shape of the distribution is in more detail

# This is my current default for boxplots for my publications, & I request it in peer reviews

## 5. scatterplot ##

# basic
ggplot(jerimalai_lithics_to_plot,
       aes(Length,
           Width)) +
  geom_point()

# add another variable with colour
ggplot(jerimalai_lithics_to_plot,
       aes(Length,
           Width,
           colour = Material)) +
  geom_point()

# too many materials! Let's filter out a bunch
jerimalai_lithics_to_plot_raw_materials <- 
jerimalai_lithics_to_plot %>% 
  filter(Material %in% c("Chert", 
                         "Volcanic",
                         "Silcrete"))

# or use point shape, only good with small number of larger points
ggplot(jerimalai_lithics_to_plot_raw_materials,
       aes(Length,
           Width,
           shape = Material)) +
  geom_point()

# add another variable with point size
ggplot(jerimalai_lithics_to_plot_raw_materials,
       aes(Length,
           Width,
           colour = Material,
           size = Weight)) +
  geom_point()

# adjust point size to an arbitrary size
ggplot(jerimalai_lithics_to_plot_raw_materials,
       aes(Length,
           Width,
           colour = Material)) +
  geom_point(size = 1)

# add a line of best fit
ggplot(jerimalai_lithics_to_plot_raw_materials,
       aes(Length,
           Width,
           colour = Material)) +
  geom_point(size = 1) +
  geom_smooth()

# we get one line per colour, if we want one line for all points
# we do this

ggplot(jerimalai_lithics_to_plot_raw_materials,
       aes(Length,
           Width
           )) +
  geom_point(aes(colour = Material),
             size = 1) +
  geom_smooth()

# default best fit line is loess, but if we want linear regression line...

ggplot(jerimalai_lithics_to_plot_raw_materials,
       aes(Length,
           Width
       )) +
  geom_point(aes(colour = Material),
             size = 1) +
  geom_smooth(method = "lm")

# and if we want to show the linear model equation on the plot...

library(ggpmisc)
my.formula <- y ~ x
ggplot(jerimalai_lithics_to_plot_raw_materials,
       aes(Length,
           Width
       )) +
  geom_point(aes(colour = Material),
             size = 1) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "black", 
              formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., 
                                 ..rr.label.., 
                                 sep = "~~~")), 
               parse = TRUE)

# and finally, apply a theme, and adjust axis titles

ggplot(jerimalai_lithics_to_plot_raw_materials,
       aes(Length,
           Width
       )) +
  geom_point(aes(colour = Material),
             size = 1) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "black", 
              formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., 
                                 ..rr.label.., 
                                 sep = "~~~")), 
               parse = TRUE) +
  theme_bw() +
  xlab("Length (mm)") +
  ylab("Width (mm)")

## bonus feature: facetting ##

ggplot(jerimalai_lithics_to_plot_raw_materials,
         aes(Width,
             Length)) +    # plot
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Material)

# say we want 1:1 scaling on the axes

ggplot(jerimalai_lithics_to_plot_raw_materials,
       aes(Width,
           Length)) +    # plot
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Material) +
  coord_equal()

# and you know the rest for axis titles, themes, etc

## bonus feature: interactivity ##

library(plotly) # excellent documentation at https://plot.ly/r/

j_silcrete <- 
jerimalai_lithics_to_plot_raw_materials %>% 
  filter(Material == "Silcrete") %>% 
  ggplot(aes(Length,
             Width)) +
  geom_point()

# presto! interactive!
ggplotly(j_silcrete)

# if we want custom info in mouse-over popup...

j_silcrete <- 
  jerimalai_lithics_to_plot_raw_materials %>% 
  filter(Material == "Silcrete") %>% 
  ggplot(aes(Length,
             Width)) +
  geom_point(aes(text = paste0("Spit: ", Spit)))

ggplotly(j_silcrete)

# amazing! super useful for interactive work
# for point labels on static plots, use the ggrepel package

# challenge: in the ktc_ceramic_data, plot mass by counts
ktc_plot <- 
ggplot(ktc_ceramic_data, 
       aes(Mass, Counts)) +
  geom_point(aes(text = paste0("EU: ", EU)))

ggplotly(ktc_plot)


## bonus feature: saving plots ##

# two methods: ggsave (recommended!!) and RStudio Plots pane

# draw plot, e.g.
j_silcrete
# then use ggsave to get PNG
ggsave(filename = "j_silcrete.png",
       width = 10,
       height = 10, 
       units = "cm",
       dpi = 600) # not less than 300 for publication
# can take some trial and error to get the size and dpi looking good

# or save as SVG and edit in Inkscape (e.g. combine with other graphics) to make PNG
ggsave(filename = "j_silcrete.svg",
       width = 10,
       height = 10, 
       units = "cm")

# plots pane, Export -> Save As image... -> adjust size, proportions and location, then save
# best for quick plots to share with collaborators


#  ---------------------------------------------------
#  ---------------------------------------------------

# Testing - let's do three very common stat tests

# for more on how to choose the right test:
# - table: http://www.biostathandbook.com/testchoice.html
# - flowchart: http://www.biochemia-medica.com/content/comparing-groups-statistical-differences-how-choose-right-statistical-test   
# - many examples in R: http://stats.idre.ucla.edu/other/mult-pkg/whatstat/ 


## Testing for the difference in measurements of two samples 

# Very common statistical test, for testing if two samples differ
# for example, length of artefacts of two different raw materials. 

jerimalai_lithics_to_plot_si_vo <- 
  jerimalai_lithics_to_plot %>% 
  filter(Material %in% c("Silcrete", 
                         "Volcanic"))

# - normality check:  visual & shapiro.test() & qqnorm()

# 1. take a look, not very normally distributed, long right tails
ggplot(jerimalai_lithics_to_plot_si_vo,
       aes(Length)) +
  geom_histogram() +
  facet_wrap( ~ Material, 
              ncol = 1,
              scales = "free_y")

# 2. statisitical test for normality of each sample

# using the purr package

j_s_test1 <- 
  jerimalai_lithics_to_plot_si_vo %>%   # take the data frame...
  split(.$Material) %>%                 # very similar to 'group_by'...
  map(~ shapiro.test(.$Length))         # '.' stands for each data frame in the split
                                        # we apply the test to the Length col of each df

# using purrr, dplyr, and broom, one more line, but tidier output!

library(broom)

j_s_test2 <- 
  jerimalai_lithics_to_plot_si_vo %>% 
  group_by(Material) %>%                 
  by_slice(~tidy(shapiro.test(.$Length))) %>% 
  unnest()

# in any case, the p-value is very low, so it confirms our visual assessment, 
# so we should use a test that does not depend on the data having a normal distribution

# 3. statistical test for difference between the two samples

wilcox.test(Length ~ Material,  # the ~ is the formula interface, we can read ~ as 'by'
            data = jerimalai_lithics_to_plot_si_vo)

# How to use the formula interface: LHS ~ RHS where LHS is a numeric variable giving the data values and RHS a factor/character with two levels giving the corresponding groups.

# another way to type it:
  
with(jerimalai_lithics_to_plot_si_vo, 
     wilcox.test(Length ~ Material))

# result is p < 0.05, so we say YES! they are significantly different in length

# if the data were normally distributed, we'd use the t-test in exactly the same way:

t.test(Length ~ Material,  # the ~ is the formula interface, we can read ~ as 'by'
            data = jerimalai_lithics_to_plot_si_vo)

# How to use the forumar interface: LHS ~ RHS where LHS is a numeric variable giving the data values and RHS a factor/character with two levels giving the corresponding groups.

with(jerimalai_lithics_to_plot_si_vo, 
     t.test(Length ~ Material))

# result is the same, but we know this is not the appropriate method for these data, so we' wouldn't publish this


#  ---------------------------------------------------
#  ---------------------------------------------------

## Testing for the difference in measurements of three or more samples 

# For example, does the Weight of the artefact vary across 4 different raw materials?

jerimalai_lithics_to_plot_4 <- 
  jerimalai_lithics_to_plot %>% 
  filter(Material %in% c("Silcrete", 
                         "Volcanic",
                         "Chert",
                         "Quartzite"))

# 1. check for the normality of the weight measurements

ggplot(jerimalai_lithics_to_plot_4,
       aes(Weight)) +
  geom_histogram() +
  facet_wrap( ~ Material, 
              ncol = 1,
              scales = "free_y") 
# very hard to see anything!
# try log scale

ggplot(jerimalai_lithics_to_plot_4,
       aes(Weight)) +
  geom_histogram() +
  facet_wrap( ~ Material, 
              ncol = 1,
              scales = "free_y") +
  scale_x_log10()

# better to see distribution when squashed up at one end (this case, the small end)

# we can also use the boxplot effectively here
ggplot(jerimalai_lithics_to_plot_4,
       aes(Material,
           Weight)) +
  geom_boxplot() +
  geom_sina(alpha = 0.01) +
  scale_y_log10()

# def not normal!

# 2. Do the stat test for differences in measurement by the groups
# for non-normal distributions, we can use the Kruskal-Wallis Rank Sum Test
# more: http://rcompanion.org/rcompanion/d_06.html

with(jerimalai_lithics_to_plot_4, 
     kruskal.test(Weight ~ as.factor(Material)))

#  post-hoc test to see where the significant difference is
#  for the non-normal situation like this, we use the Dunn test

library(FSA)

my_dunn_test <- 
with(jerimalai_lithics_to_plot_4, 
     dunnTest(Weight ~ as.factor(Material),
              method = "bh"))

# check it out
my_dunn_test
str(my_dunn_test)
# here's the data
my_dunn_test$res

# examine the data frame, and arrange by P.adj values
arrange(my_dunn_test$res, P.adj)

# so all pairs except  Silcrete - Volcanic are significantly different

## If the data were normal ##

# we'd use ANOVA for the test, need to add summary() to get p-value

my_anova <- 
with(jerimalai_lithics_to_plot_4, 
     aov(Weight ~ as.factor(Material)))

summary(my_anova)

# and for the post-hoc, we'd use Tukey's HSD

my_tukeyhsd <- TukeyHSD(my_anova)

# what is this?
my_tukeyhsd
# so... not very convieienet.. 
str(my_tukeyhsd)
# we can make it more useful
library(broom)
tidy(my_tukeyhsd)              


#  ---------------------------------------------------
#  ---------------------------------------------------

## Testing for difference in counts 

# For example, are there signficicantly differen numbers of ceramic peices in the different 
# stratigraphic contexts in KTC?

# summarise the data into a table...

ktc_ceramic_data %>% 
  group_by(Context) %>% 
  summarise(sum_counts = sum(Counts)) 

# a few problems, 'Context' col is character, and we have 7L and 7U...
# let's fix them

ktc_ceramic_data %>% 
  mutate(Context = ifelse(grepl("L|U", Context), 7, Context)) %>% 
  group_by(Context) %>% 
  summarise(sum_counts = sum(Counts)) %>% 
  mutate(Context = as.numeric(Context)) 

# better! Now we can apply the chi-square test:

ktc_ceramic_data %>% 
  mutate(Context = ifelse(grepl("L|U", Context), 7, Context)) %>% 
  group_by(Context) %>% 
  summarise(sum_counts = sum(Counts)) %>% 
  mutate(Context = as.numeric(Context)) %>% 
  chisq.test()

ktc_chi_sq <- 
  ktc_ceramic_data %>% 
  mutate(Context = ifelse(grepl("L|U", Context), 7, Context)) %>% 
  group_by(Context) %>% 
  summarise(sum_counts = sum(Counts)) %>% 
  mutate(Context = as.numeric(Context)) %>% 
  chisq.test()

ktc_chi_sq

# p-value indicates that we really do have a significant difference in the number of sherds in each stratigraphic context

# to get more information, we can look at the residuals:

ktc_chi_sq$residuals

# A general rule of thumb for figuring out what the standardized residual means, is:
# If the residual is less than -2, the cell’s observed frequency is less than the expected frequency. Greater than 2 and the observed frequency is greater than the expected frequency.

# another example, stone artefact types:

# check to see what the most numerous types are
jerimalai_lithics_to_plot_4 %>% 
  group_by(Artclas) %>% 
  tally() %>% 
  arrange(desc(n))

# do we have significant difference in flake types between flake types in each excavation unit?

jerimalai_lithics_to_plot_4 %>% 
  filter(Artclas %in% c("Flake", "`Flake-b`", "RetF", "Shatt")) %>% 
  group_by(Spit, Artclas) %>% 
  tally() %>% 
  spread(Artclas, n, fill = 0) %>% 
  ungroup() %>% 
  select(-Spit) %>% 
  chisq.test()

# keep the result
j_chi_sq <- 
jerimalai_lithics_to_plot_4 %>% 
  filter(Artclas %in% c("Flake", "`Flake-b`", "RetF", "Shatt"),
         Spit %in% 1:20) %>% 
  group_by(Spit, Artclas) %>% 
  tally() %>% 
  spread(Artclas, n, fill = 0) %>% 
  ungroup() %>% 
  select(-Spit) %>% 
  chisq.test()

# inspect the residuals can help us 
j_chi_sq$residuals

# Indonesian ceramics example 

View(bulbeck_ceramics_df)

# make meaningful column names
names(bulbeck_ceramics_df) <- bulbeck_ceramics_df[ 2 , ]

# delete non-data rows
bulbeck_ceramics_df <- bulbeck_ceramics_df[ -c(1:2), ]

# see what we have with the vessel forms
unique(bulbeck_ceramics_df$`vessel form `)

chi_sq_indo <- 
bulbeck_ceramics_df %>% 
  # select just relevant cols
  select(`vessel form `, lampetia, andomo) %>% 
  # remove space from some of the text
  mutate(`vessel form ` = gsub(" ", "", `vessel form `,),
         # make case consistent
         `vessel form ` = ifelse(`vessel form ` == 'bowl', 
                                 'Bowl', 
                                 `vessel form `),
         # coerce from character to numeric
         lampetia = as.numeric(lampetia),
         andomo = as.numeric(andomo)) %>% 
  # focus on three vessel types
  filter(`vessel form ` %in% c("Bowl", 
                               "Martavan", 
                               "Bowls,plate")) %>% 
  # wide to long
  gather(site, count, -`vessel form ` ) %>% 
  # get counts
  group_by(`vessel form `, site ) %>% 
  summarise(count = sum(count, na.rm = TRUE)) %>% 
  # long to wide for the chi-sq test
  spread(site, count) %>% 
  ungroup() %>% 
  # remove non-numeric col
  select(-`vessel form `) %>% 
  chisq.test()

# inspect result:
chi_sq_indo
# not significant, but if it were, we can look at the residuals to see where the most influential cells are:

chi_sq_indo$residuals

#  ---------------------------------------------------
#  ---------------------------------------------------

# END! 

# to recap, here's what we've done:

#- Interacting with R & RStudio, getting help
#- Importing tabular data
#- Inspecting & cleaning data
#- Exploratory Data Analysis: reshaping & summarising 
#- Exploratory Data Analysis: plotting & interactivity
#- Testing for the difference in measurements of two samples
#- Testing for the difference in measurements of three or more samples
#- Testing for difference in tables of counts


# Good luck, have fun and be bold trying new things with your data and R!
  






