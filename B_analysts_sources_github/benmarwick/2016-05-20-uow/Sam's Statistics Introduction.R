library(readxl)

xdata=read_excel("stone_artefacts.xlsx")

#Check data (see if there are missing cases or NAs)
str(xdata)
summary(xdata)
head(xdata)

#Check distribution of our variables
table(xdata$raw_material, xdata$excavation_unit)

library(dplyr)

xdata %>% 
  group_by(excavation_unit,raw_material) %>% 
  tally 

hist(xdata$length)
hist(xdata$width)
hist(xdata$thickness)

#It is usually a good idea to convert categorical variables to "factors"
xdata$raw_material=as.factor(xdata$raw_material)
xdata$excavation_unit=as.factor(xdata$excavation_unit)

#Are they normally distributed? Multiple ways and no single answer
qqnorm(xdata$length)
qqline(xdata$length)
shapiro.test(xdata$length)

qqnorm(xdata$width)
qqline(xdata$width)
shapiro.test(xdata$width)

qqnorm(xdata$thickness)
qqline(xdata$thickness)
shapiro.test(xdata$thickness)

#How would we describe the central tendency of the three dependent variables?
mean(xdata$length)
median(xdata$length)
hist(xdata$length)
abline(v=mean(xdata$length), col='red')
abline(v=median(xdata$length), col='blue')

mean(xdata$width)
median(xdata$width)
hist(xdata$width)
abline(v=mean(xdata$width), col='red')
abline(v=median(xdata$width), col='blue')

mean(xdata$thickness)
median(xdata$thickness)
hist(xdata$thickness)
abline(v=mean(xdata$thickness), col='red')
abline(v=median(xdata$thickness), col='blue')

#What if we want to summarise these statistics by species and/or sex?

#Either by raw material or excavation unit with dplyr function 
xdata %>% 
  group_by(excavation_unit) %>% 
  summarise_each(funs(mean), c(length, width, thickness)) 

xdata %>% 
  group_by(raw_material) %>% 
  summarise_each(funs(mean), c(length, width, thickness)) 

#Group by both raw material and excavation unit
xdata %>% 
  group_by(raw_material, excavation_unit) %>% 
  summarise_each(funs(mean), c(length, width, thickness)) 


#How do we describe the variance of the three dependent variables?
sd(xdata$length)
sd(xdata$width)
sd(xdata$thickness)
range(xdata$thickness)
summary(xdata$thickness)
quantile(xdata$thickness, c(0.05, 0.25, 0.5, 0.75, 0.95))

my_quantiles=quantile(xdata$thickness, c(0.05, 0.25, 0.5, 0.75, 0.95))
hist(xdata$thickness)
abline(v=my_quantiles, col="blue")

#Simple plots are also useful to explore patterns
library(ggplot2)

ggplot(xdata, 
  aes(length, width, colour = raw_material)) +
  geom_point()

ggplot(xdata, 
  aes(raw_material,length)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2,
              width = 0.3) 

ggplot(xdata, 
       aes(excavation_unit,length)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2,
              width = 0.3) 

#Testing hypotheses
#Think about your research question, what is the hypothesis and how do I test them?

#Hypothesis 1: do flake length and width share a correlation?
#parametric - Pearson's correction - only useful if correlation is linear
cor.test(xdata$length, xdata$width, method="pearson")
#non-parametric - Spearman's rank rho - useful for both linear and non-linear relationships
cor.test(xdata$length, xdata$width, method="spearman")

#what if I want to test the relationship in one kind of raw material or in a particular excavation uni?
silcrete_chert = xdata %>% filter (raw_material %in% c("silcrete", "chert"))
cor.test(silcrete_chert$length, silcrete_chert$width, method="pearson")
cor.test(silcrete_chert$length, silcrete_chert$width, method="spearman")


#Hypothesis 2: the lengths of silcrete flakes are different from chert flakes

#always a good idea to plot them first
ggplot(silcrete_chert,
  aes(raw_material, length)) +
  geom_boxplot()

#check whether my data is symmetrically distributed
hist(silcrete_chert$length)

#non-parametric two independent sample test - Mann–Whitney U test
wilcox.test(silcrete_chert$length ~ silcrete_chert$raw_material)

#parametric two independent sample test - t test
t.test(silcrete_chert$length ~ silcrete_chert$raw_material)


#Hypothesis 3: is there a difference between flake length and width?
#Question - are the samples of flake lengths and widths here independent or dependent?
#They are dependent because the length of a flake correspond to the width of that particular flake.

#non-parametric two dependent sample test
wilcox.test(silcrete_chert$length, silcrete_chert$width, paired=TRUE)

#parametric two dependent sample test - t test
t.test(silcrete_chert$length, silcrete_chert$width, paired=TRUE)

#Issue of multiple testing - avoid doing too many tests
#Test for differences across more than two groups

#again, lets plot the data first to see
ggplot(xdata, 
  aes(raw_material, length)) +
  geom_boxplot()

#side note - we have to be careful how to frame our hypothesis.
#We can look at the plot and then decide that our hypothesis is that quartzite flakes are longer than others
#This is because we are supposed to formulate our hypothesis before gathering our data
#This difference affects the way we compute our p-value. If we have a priori reason to predict that quartzite flakes would be longer,
#we would then compute a one-tailed p value in our tests. On the other hand, if we just hypothesise quartzite flakes have 'different' lengths,
#than other raw material types, then we compute a two-tailed p. Almost all statistic functions assume we are after a two-tailed p.

#parametric test for more than two samples is ANOVA
#again, ANOVA assumes the data fits a normal distribution
aov(xdata$length ~ xdata$raw_material)
my_aov=aov(xdata$length ~ xdata$raw_material)
summary(my_aov)

#non parametric test equivilant is Kruskal–Wallis test
kruskal.test(xdata$length ~ xdata$raw_material)


#Post-hoc test - so which sample(s) is/are causing the significant different in length?
#Try not to run multiple t tests because again we run into the multiple testing problem

#For ANOVA, we can use Tukey test
my_thsd <- TukeyHSD(my_aov)
plot(my_thsd)

#For Kruskal-Wallis test, we need a new library
library(PMCMR)

posthoc.kruskal.nemenyi.test(xdata$length ~ xdata$raw_material,
                             dist = "Tukey")

#Testing differences in count
#Do raw materials represent differently across the excavation units?

#Chi-Square
#we need to create a new dataframe that tally the number of different raw materials across the excavation units

library(tidyr)

count_data = 
  xdata %>% 
  group_by(excavation_unit, raw_material) %>% 
  tally %>% 
  spread(raw_material, n) 

#Take a look at the data
count_data

#we cant have NA or missing data to do chi-square
count_data_chert =
  count_data %>% 
  mutate(non_chert = sum(quartzite, silcrete, na.rm = TRUE)) %>% 
  ungroup %>% 
  select(chert, non_chert)

chisq.test(count_data_chert)


#t test assumes normality; Mann-Whitney U test assumes equal variance
#what if we dont want to assume these things?
#e.g., are the lengths of silcrete flakes different from quartzite flakes?
#lets check with Mann-Whitney U test first
silcrete_quartzite = xdata %>% filter (raw_material %in% c("silcrete", "quartzite"))

wilcox.test(silcrete_quartzite$length ~ silcrete_quartzite$raw_material)

ggplot (silcrete_quartzite,
  aes(raw_material, length)) +
  geom_boxplot()

#bootstrapping is an alternative

#first we take our two samples and combine them into one big sample
silcrete_length=xdata$length[xdata$raw_material=="silcrete"]
quartzite_length=xdata$length[xdata$raw_material=="quartzite"]
combined=c(silcrete_length, quartzite_length)

#compute the actual difference between the means of our two samples
obs.dif=mean(silcrete_length) - mean(quartzite_length)

#create an object called "dif" to record our bootstrapped mean differences
dif=0

#write a sampling loop to bootstrap two samples from the combined data
#with sample sizes equal to our original silcrete and quartzite samples
#take the difference between the means of these two bootstrapped samples and record it in the object 'dif'
#Important - bootstrapping means we are sampling from our sample WITH replacement
for (i in 1:10000) {
 x=sample(combined, replace=TRUE)
 silcrete=x[1:length(silcrete_length)]
 quartzite=x[length(silcrete_length+1):length(combined)]
 dif[i]=mean(silcrete)-mean(quartzite)
}

#Take a look at our bootstrapped mean differences
hist(dif)
abline(v=obs.dif, col="blue")
#Count up the number of times that our bootstrapped mean difference is bigger than the actua mean difference between our two samples
table(abs(dif)>=abs(obs.dif))
mean(abs(dif) >= abs(obs.dif))


# thoughts ----------------------------------------------------------------

# This all looks excellent

# line 13ff, you could reproduce the table() output with another line,
# on the dplyr function. 
# The advantage of the dplyr/tidyr method is that you get a data frame
# as output. 

library(dplyr)
library(tidyr)

xdata %>% 
  group_by(excavation_unit,raw_material) %>% 
  tally  %>% 
  spread(excavation_unit, n)


# Just as an aside, your cor.test example made me think of this
# very nice package called broom that tidies 
# the output of many common tests into dataframes. There's a very detailed 
# vignette here: https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
# This is handy because cor.test and many other functions don't give dataframe
# output, and we often want dataframes to filter, sort or display the output
# Here's how broom could be used in an example of computing the 
# correlation of length and width within each raw material type 
# for silcrete and chert:


library(broom) # for the tidy() function

xdata %>% 
  filter(raw_material %in% c("silcrete", "chert")) %>% 
  group_by(raw_material) %>% 
  do(tidy(cor.test(.$length, .$width)))

# It's also good for the output of the TukeyHSD() test, which is 
# difficult to read when there are more than 3-4 groups and when we
# want to filter to see the significant comparisons:

my_thsd <- TukeyHSD(my_aov)
my_thsd %>% 
  tidy %>% 
  filter(adj.p.value < 0.05)


# No need to show any of this in the workshop, I just thought just might find it
# interesting. We could use it in a future workshop on modelling, broom is excellent
# at working with lm() and related functions. 


