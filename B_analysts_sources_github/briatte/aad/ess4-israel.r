## European Social Survey Round 4 example, using some code by Anthony J. Damico.
## 2015-11-17

## -- reference ----------------------------------------------------------------

# European Social Survey (2010). ESS Round 4 (2008/2009) Final Activity Report.
# London: Centre for Comparative Social Surveys, City University London.

## -- packages -----------------------------------------------------------------

library(ggplot2)
library(survey)

## -- dataset ------------------------------------------------------------------

ess4 = read.csv("ess4-israel.csv", stringsAsFactors = FALSE)

# number of rows (observations) in the cross-sectional data set
nrow(ess4)

# survey design object with ESS design information (Taylor-series linearization)
# see data documentation report for sampling frame, page 78 for details
ess = svydesign(
  ids = ~ psu,
  strata = ~ stratify,
  probs = ~ prob,
  data = ess4
)

# variable transformations
ess = update(
  one = 1,
  torture = factor(trrtort, labels = c("Agree", 2, "Neither", 4, "Disagree")),
  age = agea,
  sex = factor(ifelse(gndr == 1, "Male", "Female")),
  age4 = cut(age, breaks = c(15, 24, 44, 64, 99), include.lowest = TRUE),
  religion = factor(rlgdnm),
  income10 = hinctnta,
  ess
)

# age range and quantiles
svyquantile(
  ~ age,
  design = ess,
  c(0 , .25, .5, .75, 1),
  na.rm = TRUE
)

# average age by gender
svyby(
  ~ age,
  ~ sex,
  design = ess,
  svymean,
  na.rm = TRUE
)

# education range and quantiles
svyquantile(
  ~ eduyrs,
  design = ess,
  c(0 , .25, .5, .75, 1),
  na.rm = TRUE
)

# left-right range and quantiles
svyquantile(
  ~ lrscale,
  design = ess,
  c(0 , .25, .5, .75, 1),
  na.rm = TRUE
)

# proportions of income groups
svymean(~ factor(income10), ess, na.rm = TRUE)

# proportions of religious groups
svymean(~ religion, ess, na.rm = TRUE)

## -- analysis -----------------------------------------------------------------

# dependent variable (DV), unweighted counts
x = svyby(
  ~ one,
  ~ torture,
  design = ess,
  unwtd.count,
  na.rm = TRUE
)

x

# dependent variable (DV), weighted proportions
x = svymean(
  ~ torture,
  design = ess,
  na.rm = TRUE
)

x

# plot
ggplot(data = as.data.frame(x), aes(factor(1:5), mean)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Agree", "", "Neither", "", "Disagree")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "\nTorture in country never justified...", y = "Responses\n")

# distribution of answers in each gender group, unweighted counts
t = svyby(
  ~ one,
  ~ sex + torture,
  design = ess,
  unwtd.count,
  na.rm = TRUE
)

data.frame(t, row.names = NULL)

# distribution of answers in each gender group, weighted proportions
t = svyby(
  ~ torture,
  ~ sex,
  design = ess,
  svymean,
  na.rm = TRUE
)

t[, 1:6 ] # row percentages

# prepare table for plot
t = reshape2::melt(t[, 1:6], id = "sex", variable = "torture")
t$torture = gsub("torture", "", t$torture)
t$torture = factor(t$torture, levels = c("Agree", 2, "Neither", 4, "Disagree"))

# plot
ggplot(data = t, aes(sex, value, fill = torture)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer("", palette = "RdYlBu") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Responses\n", x = NULL)

# average age per response item, by sex
t = svyby(
  ~ age,
  ~ torture + sex,
  design = ess,
  svymean,
  na.rm = TRUE
)

t

# plot
ggplot(data = t, aes(x = torture, y = age)) +
  geom_point() +
  geom_segment(aes(xend = torture,
                   y = age - 2 * se, yend = age + 2 * se)) +
  scale_y_continuous(lim = c(15, 50)) + # from svyquantile
  scale_x_discrete(labels = c("Agree", "", "Neither", "", "Disagree")) +
  labs(x = "\nTorture in country never justified...", y = "Mean age\n") +
  facet_wrap(~ sex)

# average income band per response item, by sex
t = svyby(
  ~ income10,
  ~ torture + sex,
  design = ess,
  svymean,
  na.rm = TRUE
)

t

# plot
ggplot(data = t, aes(x = torture, y = income10)) +
  geom_point() +
  geom_segment(aes(xend = torture,
                   y = income10 - 2 * se, yend = income10 + 2 * se)) +
  scale_y_continuous(lim = c(0, 10)) +
  scale_x_discrete(labels = c("Agree", "", "Neither", "", "Disagree")) +
  labs(x = "\nTorture in country never justified...",
       y = "Mean income band (1-10)\n") +
  facet_wrap(~ sex)

# average education per response item, by sex
t = svyby(
  ~ eduyrs,
  ~ torture + sex,
  design = ess,
  svymean,
  na.rm = TRUE
)

t

# plot
ggplot(data = t, aes(x = torture, y = eduyrs)) +
  geom_point() +
  geom_segment(aes(xend = torture,
                   y = eduyrs - 2 * se, yend = eduyrs + 2 * se)) +
  #scale_y_continuous(lim = c(0, 10)) +
  scale_x_discrete(labels = c("Agree", "", "Neither", "", "Disagree")) +
  labs(x = "\nTorture in country never justified...",
       y = "Mean education (years)\n") +
  facet_wrap(~ sex)

# average political position per response item, by sex
t = svyby(
  ~ lrscale,
  ~ torture + sex,
  design = ess,
  svymean,
  na.rm = TRUE
)

t

# plot
ggplot(data = t, aes(x = torture, y = lrscale)) +
  geom_point() +
  geom_segment(aes(xend = torture,
                   y = lrscale - 2 * se, yend = lrscale + 2 * se)) +
  #scale_y_continuous(lim = c(0, 10)) +
  scale_x_discrete(labels = c("Agree", "", "Neither", "", "Disagree")) +
  labs(x = "\nTorture in country never justified...",
       y = "Mean political position (0-10)\n") +
  facet_wrap(~ sex)

# average response per religious denomination, by sex
t = svyby(
  ~ torture,
  ~ religion,
  design = ess,
  svymean,
  na.rm = TRUE
)

t[, 1:6 ] # row percentages

# prepare table for plot
t = reshape2::melt(t[, 1:6], id = "religion", variable = "torture")
t$torture = gsub("torture", "", t$torture)
t$torture = factor(t$torture, levels = c("Agree", 2, "Neither", 4, "Disagree"))

# plot
ggplot(data = subset(t, religion %in% 5:6), aes(x = religion, y = value)) +
  geom_bar(stat = "identity", aes(fill = torture)) +
  scale_x_discrete(labels = c("Jewish", "Muslim")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer("", palette = "RdYlBu") +
  labs(x = NULL, y = "Responses (%)\n")

## have a nice day
