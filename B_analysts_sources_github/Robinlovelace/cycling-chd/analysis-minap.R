##########################################
######## Cross-sectional Analyses ########
##########################################


# Libraries
library(data.table)
library(plyr)

library(MASS) # For Negative Binomial Regression
library(AER) # Test for over-dispersion
library(pscl) # Test for over-dispersion

# Load data
# minap_msoas <- readRDS("data/msoas_observed_expected_counts.Rds")
source("R/process-minap.R")

# Aggregate by MSOA
dt <- data.table(minap_msoas)
msoa_persons <- dt[, list(admissions = sum(admissions, na.rm = TRUE), expt_adms = sum(expt_adms, na.rm = TRUE)),
                    by = c("msoa_code")]
msoa_sex <- dt[, list(admissions = sum(admissions, na.rm = TRUE), expt_adms = sum(expt_adms, na.rm = TRUE)),
                   by = c("sex", "msoa_code")]
msoa_males <- msoa_sex[msoa_sex$sex=="Male"]
msoa_females <- msoa_sex[msoa_sex$sex=="Female"]
rm(minap_msoas)
rm(msoa_sex)
rm(dt)
gc()

# Load transport data for MSOAs
msoa_transport <- readRDS("data/msoas.Rds") # Load
msoa_transport$msoa_code <- msoa_transport$geo_code
msoa_transport$geo_code <- NULL

# Calculate exposure variables
msoa_transport$pc_cycle <- (msoa_transport$Bicycle / msoa_transport$All) * 100 # cycle
msoa_transport$pc_walk <- (msoa_transport$foot / msoa_transport$All) * 100 # walk
msoa_transport$pc_car <- (msoa_transport$Car / msoa_transport$All) * 100 # car
# msoa_transport <- msoa_transport[,5:8] # drop variables not needed

# Join on cycling data
msoa_p <- join(msoa_persons, msoa_transport@data, by = c("msoa_code"), type = "left", match = "all")
msoa_m <- join(msoa_males, msoa_transport@data, by = c("msoa_code"), type = "left", match = "all")
msoa_f <- join(msoa_females, msoa_transport@data, by = c("msoa_code"), type = "left", match = "all")
rm(msoa_transport)
rm(msoa_persons)
rm(msoa_females)
rm(msoa_males)

# Drop missing data (i.e. only england MSOAs - n=6147)
eng_p <- na.omit(msoa_p)
eng_m <- na.omit(msoa_m)
eng_f <- na.omit(msoa_f)
rm(msoa_p)
rm(msoa_f)
rm(msoa_m)





##### Statistical Analysis #####


### Persons level analysis ###


# Check distribution of outcome variable
hist(eng_p$admissions)
summary(eng_p$admissions) # Note no MSOAs with 0 admissions

## Poisson regression model ##
model_p <- glm(admissions ~ pc_cycle, family = "poisson", data = eng_p, offset = log(expt_adms))

# Goodness of Fit test [chi-square test based on the residual deviance and degrees of freedom]
1 - pchisq(summary(model_p)$deviance,    # We want this to be p > 0.05
           summary(model_p)$df.residual) # If p>0.05 then suggests Poisson model fits data well

# GOF 2
qchisq(0.95, df.residual(model_p)) # Get five-percent critical value for a chi-squared with df from model
deviance(model_p) # we want the deviance lower than the above number
pr <- residuals(model_p,"pearson") # Pearsons chi square
sum(pr^2) # also want this lower


## Negative Binomial Regression ##
model_nb <- glm.nb(admissions ~ pc_cycle + offset(expt_adms), data = eng_p)

# Goodness of fit (improvement from Poisson model)
1 - pchisq(summary(model_nb)$deviance,
           summary(model_nb)$df.residual)

qchisq(0.95, df.residual(model_nb))
deviance(model_nb)
pr <- residuals(model_nb,"pearson")
sum(pr^2)


## Test model assumptions ##
dispersiontest(model_p, trafo=1) # Overdispersion present in larger than 0 (which it is)
odTest(model_nb) # compares log-likelihood ratios of NegBin model to Poisson approach - here we can reject the Poisson model in favour of NegBin (i.e. p significant)
AIC(model_p, model_nb) # lower is better model
vuong(model_p, model_nb) # model which is significant is better


## Results ##
summary(model_nb)
cbind(exp(coef(model_nb)), exp(confint(model_nb))) # Convert to IRRs (take p from summary(model_nb))



### Males analysis ###


# Check distribution of outcome variable
hist(eng_m$admissions) # hist(eng_m$admissions[eng_m$admissions<30]) easier to see
summary(eng_m$admissions)

## Poisson regression model ##
model_p <- glm(admissions ~ pc_cycle, family = "poisson", data = eng_m, offset = log(expt_adms))

# Goodness of Fit test [chi-square test based on the residual deviance and degrees of freedom]
1 - pchisq(summary(model_p)$deviance,    # We want this to be p > 0.05
           summary(model_p)$df.residual) # If p>0.05 then suggests Poisson model fits data well

# GOF 2
qchisq(0.95, df.residual(model_p)) # Get five-percent critical value for a chi-squared with df from model
deviance(model_p) # we want the deviance lower than the above number
pr <- residuals(model_p,"pearson") # Pearsons chi square
sum(pr^2) # also want this lower


## Negative Binomial Regression ##
model_nb <- glm.nb(admissions ~ pc_cycle + offset(expt_adms), data = eng_m)

# Goodness of fit (improvement from Poisson model)
1 - pchisq(summary(model_nb)$deviance,
           summary(model_nb)$df.residual)

qchisq(0.95, df.residual(model_nb))
deviance(model_nb)
pr <- residuals(model_nb,"pearson")
sum(pr^2)


## Test model assumptions ##
dispersiontest(model_p, trafo=1) # Overdispersion present in larger than 0 (which it is)
odTest(model_nb) # compares log-likelihood ratios of NegBin model to Poisson approach - here we can reject the Poisson model in favour of NegBin (i.e. p significant)
AIC(model_p, model_nb) # lower is better model
vuong(model_p, model_nb) # no difference


## Zero inflated NegBin model ##
model_zi <- zeroinfl(admissions ~ pc_cycle, data = eng_m, offset = log(expt_adms), dist = "negbin", EM = T)
AIC(model_nb, model_zi) # Zi model appears better but not entirely clear
vuong(model_nb, model_zi)


## Results ##
# Method seems to matter so not sure which is better
summary(model_nb)
cbind(exp(coef(model_nb)), exp(confint(model_nb))) # Convert to IRRs (take p from summary(model_nb))

summary(model_zi)
cbind(exp(coef(model_zi)), exp(confint(model_zi))) # Convert to IRRs (take p from summary(model_nb))




### Females analysis ###


# Check distribution of outcome variable
hist(eng_f$admissions) # hist(eng_m$admissions[eng_m$admissions<30]) easier to see
summary(eng_f$admissions)

## Poisson regression model ##
model_p <- glm(admissions ~ pc_cycle, family = "poisson", data = eng_f, offset = log(expt_adms))

# Goodness of Fit test [chi-square test based on the residual deviance and degrees of freedom]
1 - pchisq(summary(model_p)$deviance,    # We want this to be p > 0.05
           summary(model_p)$df.residual) # If p>0.05 then suggests Poisson model fits data well

# GOF 2
qchisq(0.95, df.residual(model_p)) # Get five-percent critical value for a chi-squared with df from model
deviance(model_p) # we want the deviance lower than the above number
pr <- residuals(model_p,"pearson") # Pearsons chi square
sum(pr^2) # also want this lower


## Negative Binomial Regression ##
model_nb <- glm.nb(admissions ~ pc_cycle + offset(expt_adms), data = eng_f)

# Goodness of fit (improvement from Poisson model)
1 - pchisq(summary(model_nb)$deviance,
           summary(model_nb)$df.residual)

qchisq(0.95, df.residual(model_nb))
deviance(model_nb)
pr <- residuals(model_nb,"pearson")
sum(pr^2)


## Test model assumptions ##
dispersiontest(model_p, trafo=1) # Overdispersion present in larger than 0 (which it is)
odTest(model_nb) # compares log-likelihood ratios of NegBin model to Poisson approach - here we can reject the Poisson model in favour of NegBin (i.e. p significant)
AIC(model_p, model_nb) # lower is better model
vuong(model_p, model_nb) # no difference


## Zero inflated NegBin model ##
model_zi <- zeroinfl(admissions ~ pc_cycle, data = eng_f, offset = log(expt_adms), dist = "negbin", EM = T)
AIC(model_nb, model_zi) # Zi model appears better but not entirely clear
vuong(model_nb, model_zi)


## Results ##
# Method seems to matter so not sure which is better
summary(model_nb)
cbind(exp(coef(model_nb)), exp(confint(model_nb))) # Convert to IRRs (take p from summary(model_nb))

summary(model_zi)
cbind(exp(coef(model_zi)), exp(confint(model_zi))) # Convert to IRRs (take p from summary(model_nb))

