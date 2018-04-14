###--------------------------------------------------
### Example factor jitplot
### Good for relatively small N datasets
###--------------------------------------------------



library(ggplot2)
library(plyr)

theme_set(theme_minimal())

### Fake data following the general pattern of Omar's example
### ggplot likes data in long format best.
Group <- c("African American/Black", "American Indian/Alaska Native", "Asian American",
           "Mexican American/Chicano", "Other", "Other Latino", "Puerto Rican", "White/Caucasian")
N <- as.list(c(12, 1, 19, 11, 5, 6, 3, 55))
obs <- sapply(N, rnorm)
names(obs) <- Group
data <- ldply (obs, data.frame)
colnames(data) <- c("Group", "Difference")
data$Group <- as.factor(data$Group)

### Plot: order factor by mean level of difference score (won't do
### much in this example because the fake data are normally distributed)

## Control jitter -- we only really want the jitter on the vertical axis,
## which isn't measuring anything
jit <- position_jitter(height=0.15, width=0.01)

p <- ggplot(data, aes(y=reorder(Group, Difference, mean), x=Difference, color=Group))

p + geom_jitter(alpha=0.5, position=jit) + labs(y="") +
    guides(color = FALSE)
