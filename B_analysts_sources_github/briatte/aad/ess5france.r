## European Social Survey Round 5 data example, by Anthony J. Damico.
## 2014-10-16

## -- reference ----------------------------------------------------------------

# European Social Survey (2012). ESS Round 5 (2010/2011) Technical Report. 
# London: Centre for Comparative Social Surveys, City University London.

## -- packages -----------------------------------------------------------------

library(ggplot2)
library(survey)

## -- dataset ------------------------------------------------------------------

ess5 = read.csv("ess5france.csv")

# survey design object with ESS design information (Taylor-series linearization)
# see page 8 of data documentaton report for sampling frame, page 78 for details
ess = svydesign(
  ids = ~ psu,
  strata = ~ stratify,
  probs = ~ prob,
  data = ess5
)

# number of rows (observations) in the cross-sectional cumulative data set
nrow(ess)

## -- variables ----------------------------------------------------------------

# average age
svymean( 
  ~ agea, 
  design = ess,
  na.rm = TRUE
)

# age range and quantiles
svyquantile( 
  ~ agea, 
  design = ess,
  c(0 , .25, .5, .75, 1),
  na.rm = TRUE
)

# gender distribution
svymean( 
  ~ factor(gndr), 
  design = ess,
  na.rm = TRUE
)

# age by sex
svyby( 
  ~ agea,
  ~ factor(gndr),
  design = ess,
  svymean,
  na.rm = TRUE
)

# variable transformations
ess =
  update( 
    gndr = ifelse(gndr == 1, "Male", "Female"),
    mnrgtjb = factor(mnrgtjb),
    wmcpwrk = factor(wmcpwrk),
    ess
  )

## -- analysis -----------------------------------------------------------------

# dependent variable (DV)
x = svymean( 
  ~ mnrgtjb, 
  design = ess,
  na.rm = TRUE
)

x

# plot
qplot(data = as.data.frame(x), x = factor(1:5), y = 100 * mean,
  stat = "identity", position = "dodge", geom = "bar") +
  scale_x_discrete(labels = c("Agree", "", "Neither", "", "Disagree")) +
  labs(x = "\nMen should have more right to job than women when jobs are scarce",
       y = "percent\n")

# average age across DV
y = svyby( 
  ~ agea,
  ~ mnrgtjb,
  design = ess,
  svymean,
  na.rm = TRUE
)

y

# plot
qplot(data = y, x = mnrgtjb, y = agea, geom = "point") +
  geom_segment(aes(xend = mnrgtjb, y = agea - 2 * se, yend = agea + 2 * se)) +
  scale_y_continuous(lim = c(14, 93)) + # from svyquantile
  scale_x_discrete(labels = c("Agree", "", "Neither", "", "Disagree")) +
  labs(x = "\nMen should have more right to job than women when jobs are scarce",
       y = "mean age\n")

# by gender
z = svyby( 
  ~ gndr,
  ~ mnrgtjb,
  design = ess,
  svymean,
  na.rm = TRUE
)

z

# sex delta (in percentage points)
z$delta = with(z, 100 * (gndrMale - gndrFemale))

# plot
qplot(data = z, x = mnrgtjb, y = delta,
      stat = "identity", position = "dodge", geom = "bar") +
  scale_x_discrete(labels = c("Agree", "", "Neither", "", "Disagree")) +
  labs(x = "\nMen should have more right to job than women when jobs are scarce",
       y = "% male - % female\n")

## have a nice day
