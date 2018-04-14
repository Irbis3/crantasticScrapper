## National Health Interview Survey 2012 example, by Anthony Damico.
## 2014-11-06

## -- reference ----------------------------------------------------------------

# Anthony J. Damico, "Analyze the National Health Interview Survey (NHIS)
# with R": http://www.asdfree.com/2012/10/analyzing-national-health-interview.html

## -- packages -----------------------------------------------------------------

library(ggplot2)
library(survey)

## -- dataset ------------------------------------------------------------------

nhis = na.omit(read.csv("nhis2012.csv"))

# inspect
str(nhis)

## -- variables ----------------------------------------------------------------

# Body Mass Index
nhis$bmi = with( nhis, weight * 703 / height^2 )

# inspect
head(nhis)
tail(nhis)

options( survey.lonely.psu = "adjust" )
nhissvy = svydesign(
  id = ~ psu , 
  strata = ~ strata ,
  nest = TRUE ,
  weights = ~ wtfa ,
  data = nhis
)

#
# calculate the mean of a continuous variable
#

# average bmi - nationwide
svymean( 
	~ bmi , 
	design = nhissvy
)

# by region of the country
svyby( 
	~ bmi , 
	~ region ,
	design = nhissvy ,
	svymean
)

#
# calculate the distribution of a categorical variable
#

# race/ethnicity - nationwide
svymean( 
	~ race ,
  na.rm = TRUE,
	design = nhissvy
)

# by region of the country
svyby( 
	~ race , 
	~ region ,
	design = nhissvy ,
	svymean,
  na.rm = TRUE
)

#
# calculate the median and other percentiles
#

# minimum, 25th, 50th, 75th, maximum 
svyquantile( 
	~ bmi , 
	design = nhissvy ,
	c( 0 , .25 , .5 , .75 , 1 )
)

# by region of the country
svyby( 
	~ bmi , 
	~ region ,
	design = nhissvy ,
	svyquantile ,
	c( 0 , .25 , .5 , .75 , 1 ) ,
	keep.var = FALSE
)

#
# subsetting example
#

# restrict the nhissvy object to females only
nhissvy.female = subset(
  nhissvy ,
  grepl("Female", sex)
)

# average bmi - nationwide, restricted to females
svymean( 
  ~ bmi , 
  na.rm = TRUE,
	design = nhissvy.female
)

## -- plots --------------------------------------------------------------------

g = svyby( 
  ~ bmi , 
  ~ race + region + sex ,
  design = nhissvy ,
  svymean,
  na.rm = TRUE
)

# plot elements
qplot(data = g, x = race, y = bmi, 
      ymin = bmi - 2 * se, ymax = bmi + 2 * se, 
      colour = race, geom = "pointrange") + # estimates at ± 2 sd
  geom_pointrange(aes(ymin = bmi - 3 * se, ymax = bmi + 3 * se), 
                  alpha = .5) + # estimates at ± 3 sd
  scale_colour_brewer(palette = "Set1") +
  facet_grid(sex ~ region) + # small multiples
  geom_hline(yintercept = svymean(~ bmi, na.rm = TRUE, design = nhissvy)[1],
             linetype = "dotted") + # sample mean
  theme_linedraw(12) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  labs(y = "BMI\n", x = NULL)

# uncomment to save
# ggsave("nhis-demographics.png", width = 11, height = 8)

# sample distribution
qplot(data = nhis, x = bmi, geom = "histogram")

# small multiples
qplot(data = nhis, x = bmi, geom = "histogram") +
  facet_wrap(~ race, ncol = 2)

# density curves
qplot(data = nhis, x = bmi, colour = race, geom = "density") +
  scale_colour_brewer("", palette = "Set1") +
  facet_grid(sex ~ .) +
  labs(y = NULL, x = "\nBMI") +
  theme_linedraw(12)

# uncomment to save
# ggsave("nhis-density.png", width = 11, height = 8)

## -- normal distribution plots ------------------------------------------------

# sample size effect
qplot(sample(nhis$bmi, 10^2), geom = "density")
qplot(sample(nhis$bmi, 10^3), geom = "density")
qplot(sample(nhis$bmi, 10^4), geom = "density")

# plot against normal curve
qplot(nhis$bmi, geom = "density") + 
  stat_function(fun = dnorm, args = list(
    mean = mean(nhis$bmi, na.rm = TRUE), 
    sd = sd(nhis$bmi, na.rm = TRUE)
  ), colour = "red")

# logarithmic transformation
nhis$log_bmi = log(nhis$bmi)

# plot against normal curve
qplot(nhis$log_bmi, geom = "density") + 
  stat_function(fun = dnorm, args = list(
    mean = mean(nhis$log_bmi, na.rm = TRUE), 
    sd = sd(nhis$log_bmi, na.rm = TRUE)
  ), colour = "red")

## have a nice day
