

# Interacting with R & RStudio --------------------------------------------

# Here we are

# what is the console? Script editor? 
# how to get help with ?, eg ?read_excel and ??group_by
# run the examples in the help files to learn how to use functions
# searching for help onlin with google: use "r help  [your search words]"
# browse the Q&A at stackoverflow.com and ask your own questions 


# Read in data from Excel -------------------------------------------------

# Assuming that:
# - we are using an RStudio Project (which sets the workind directory for us)
# - that this script file is saved in the same folder that contains the Excel file 

library(readxl)
stone_artefacts <- read_excel("stone_artefacts.xlsx")

# explore data a little 

library(dplyr)

# how many artefacts of each raw material in total?
stone_artefacts %>% 
  count(raw_material) 

# how many artefacts of each raw material in each ex unit?
stone_artefacts_rm <- 
  stone_artefacts %>% 
  group_by(excavation_unit,
           raw_material) %>% 
  tally() 

library(tidyr)
# long table (good for plotting and stat tests)
stone_artefacts_rm_long <- 
  stone_artefacts %>% 
  group_by(excavation_unit, raw_material) %>% 
  tally 

# wide table, good for publication
stone_artefacts_rm_wide <- 
  stone_artefacts_rm_long %>% 
  spread(raw_material, n) 

# export to file
write.csv(stone_artefacts_rm_wide, "stone_artefacts_rm_eu.csv")


# PLotting data with ggplot2 ----------------------------------------------

# plot

library(ggplot2)

####  points and theme
ggplot(stone_artefacts, 
       aes(length, 
           width)) +
  geom_point() +
  theme_bw()

# colour (could be shape)
ggplot(stone_artefacts, 
       aes(x = length, 
           y = width,
           colour = raw_material)) +
  geom_point(size = 3)

# want interactive?
library(plotly)
p <- ggplot(stone_artefacts, 
            aes(x = length, 
                y = width,
                colour = raw_material)) +
  geom_point(size = 3)
ggplotly(p)


# stats 
ggplot(stone_artefacts, 
       aes(length, 
           width)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# stat by group
ggplot(stone_artefacts, 
       aes(length, 
           width,
           colour = raw_material)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# facet
ggplot(stone_artefacts, 
       aes(length, 
           width)) +
  geom_point() +
  stat_smooth() +
  facet_wrap( ~ raw_material)

####   density 

ggplot(stone_artefacts, 
       aes(length)) +
  geom_density()

# separate line for each colour
ggplot(stone_artefacts, 
       aes(length, 
           colour = raw_material)) +
  geom_density()

# or facet 
ggplot(stone_artefacts, 
       aes(length)) +
  geom_density() +
  facet_wrap( ~ raw_material, ncol = 1)

####   bars
ggplot(stone_artefacts_rm, 
       aes(excavation_unit, 
           n)) +
  geom_bar(stat="identity")

# stacked with fill

ggplot(stone_artefacts_rm, 
       aes(excavation_unit, 
           n,
           fill = raw_material)) +
  geom_bar(stat="identity")

# with different colours
library(wesanderson)
ggplot(stone_artefacts_rm, 
       aes(excavation_unit, 
           n,
           fill = raw_material)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = wes_palette("GrandBudapest"))

# a better colour scheme 
library(viridis)
ggplot(stone_artefacts_rm, 
       aes(excavation_unit, 
           n,
           fill = raw_material)) +
  geom_bar(stat="identity") +
  scale_fill_viridis(discrete = TRUE) 

# axis labels and legend title
ggplot(stone_artefacts_rm, 
       aes(excavation_unit, 
           n,
           fill = raw_material)) +
  geom_bar(stat="identity") +
  scale_fill_viridis(discrete = TRUE,
                     name = "Raw material") +
  xlab("Excavation unit") +
  ylab("Number of artefacts") +
  theme_minimal()


# dodged with fill
ggplot(stone_artefacts_rm, 
       aes(excavation_unit, 
           n,
           fill = raw_material)) +
  geom_bar(stat = "identity",
           position = "dodge")

#### boxplot
ggplot(stone_artefacts, 
       aes(as.character(excavation_unit), 
           length)) +
  geom_boxplot()

# with jitter
ggplot(stone_artefacts, 
       aes(as.character(excavation_unit), 
           length)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2,
              width = 0.3)

# with scale transform of axis
library(scales)
ggplot(stone_artefacts, 
       aes(as.character(excavation_unit), 
           length)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2,
              width = 0.3) +
  scale_y_continuous(trans = log2_trans())

# by group
ggplot(stone_artefacts, 
       aes(as.character(excavation_unit), 
           length,
           colour = raw_material)) +
  geom_boxplot()

# facet
ggplot(stone_artefacts, 
       aes(as.character(excavation_unit), 
           length)) +
  geom_boxplot() +
  facet_wrap( ~ raw_material) +
  xlab("excavation unit")


# Testing for the difference in measurements of two samples ---------------

# is it normally distributed?
shapiro.test(stone_artefacts$length)  
qqnorm(stone_artefacts$length)
qqline(stone_artefacts$length)

# we will use only two groups, because that's what thet t-test is for
stone_artefacts_1_2 <- stone_artefacts %>% 
  filter(excavation_unit %in% 1:2)

# t-test (if data are normal)
t.test(data = stone_artefacts_1_2,
       length ~ excavation_unit)

# Wilcox (if data are not normal)
wilcox.test(data = stone_artefacts_1_2,
            length ~ excavation_unit)


# Testing for the difference in measurements of three or more samp --------

# explore
ggplot(stone_artefacts, 
       aes(as.character(excavation_unit), 
           length)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2,
              width = 0.3)

# normality 
shapiro.test(stone_artefacts$length)  
qqnorm(stone_artefacts$length)

# ANOVA
stone_artefacts$excavation_unit <- 
  as.factor(stone_artefacts$excavation_unit) 

my_aov <- aov(data = stone_artefacts, 
              length ~ excavation_unit)
summary(my_aov)

# post-hoc test 
my_thsd <- TukeyHSD(my_aov)
plot(my_thsd)
my_thsd$excavation_unit %>% 
  data.frame %>% 
  mutate(comparisons = row.names(.)) %>% 
  filter(p.adj < 0.05)

# If data are non-normal, use the Kruskal–Wallis test
my_kw <- kruskal.test(data = stone_artefacts, 
                      length ~ excavation_unit)

# post-hoc
library(PMCMR)
posthoc.kruskal.nemenyi.test(data = stone_artefacts, 
                             length ~ excavation_unit,
                             dist = "Tukey")


# Testing for difference in counts  ---------------------------------------


# diff number of artefacts for each EU?
stone_artefacts_rm_eu <- 
  stone_artefacts %>% 
  group_by(excavation_unit, raw_material) %>% 
  tally %>% 
  spread(raw_material, n) 

# can't have NA, so chert v non-chert
chert_non_chert <- 
  stone_artefacts_rm_eu %>% 
  mutate(non_chert = sum(quartzite, silcrete, na.rm = TRUE)) %>% 
  ungroup %>% 
  select(chert, non_chert)

# chi-sq test
chisq.test(chert_non_chert)






