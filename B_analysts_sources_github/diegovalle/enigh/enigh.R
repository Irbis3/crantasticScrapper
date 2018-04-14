########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Thu Juk 17 21:37:03 2013
## Email: diegovalle at gmail.com
## Purpose: Analyze the ENIGH
## Copyright (c) Diego Valle-Jones. All rights reserved
library(stringr)
library(ggplot2)
library(scales)
library(foreign)
library(ineq)
library(gtools)
library(survey)
library(bigvis)


## Download from http://www3.inegi.org.mx/sistemas/microdatos2/Descargas.aspx?sr=Microdatos_archivos/enigh/new/2012/tra/DBF/Tra_Concentrado_2012_concil_2010_DBF.exe&ht=02
enigh <- read.dbf(file.path("data","Concen.dbf"))
enigh$CVE_ENT <- as.numeric(str_sub(enigh$ubica_geo, 1, 2))
  
## Maybe for a two stage cluster design I should use
## folioviv and upm (unidad principal de muestreo) as id??
## So maybe the SE are wrong??
dclust <- svydesign(id = ~ upm + folioviv, strata = ~est_dis,
                    weights = ~factor_hog, data = enigh)
## According to the INEGI the mean current income is 38,125
svymean(~ing_cor, dclust)
## The decile edges, if we take the mean of the interval it should match the
## INEGI data
svyquantile(~ing_cor, dclust, seq(.1, .9, by = .1), ci = TRUE)
## The 1%
svyquantile(~ing_cor, dclust, .99, ci=TRUE)

## Gini coefficient with the crappy INEGI decile method
inegi.dec <- c(6997, 11794, 15734, 19513, 23914, 28862, 35570,44849,61014,133003)
Gini(inegi.dec)  ## Should be .44

## Gini with a ridiculous number of quantiles to simulate having the
## whole dataset
ineq <- svyquantile(~ing_cor, dclust, seq(0, 1, by = .0001))
plot(Lc(ineq))
Gini(ineq)

## Taking into account the size of the households
ineq <- svyquantile(~I(ing_cor/tot_integ), dclust, seq(0, 1, by = .0001))
plot(Lc(ineq))
Gini(ineq)

inc.state <- svyby(~ing_cor, ~CVE_ENT, dclust, svymean)
states <- read.csv(file.path("data", "states.csv"))
inc.state <- merge(inc.state, states, by = "CVE_ENT")

ggplot(inc.state, aes(reorder(NOM_ENT, ing_cor), ing_cor)) +
  coord_flip() +
  geom_errorbar(aes(ymin=ing_cor-se, ymax=ing_cor+se),
                colour="red", width=.1) +
  geom_point()





## Big big vector of 31 million homes. Better use the method with survey weights
total.homes <- rep(enigh$ing_cor, enigh$factor_hog)
per.capita <- rep(enigh$ing_cor/enigh$tot_integ, enigh$factor_hog)

## Check that the data matches the INEGI table
total.homes <- rev(sort(total.homes))
## This should be 133003
mean(total.homes[1:(31559379/10)])

## Gini with a 31 million vector
Gini(total.homes)
## Now taking into account the number of persons living in each home
Gini(per.capita)

## A quick plot of how income is distributed in 31 million homes
cond <- condense(bin(total.homes))
autoplot(smooth(cond, 3000)) +
  scale_x_log10(labels=dollar) +
  geom_vline(xintercept = median(total.homes), color = "red")
