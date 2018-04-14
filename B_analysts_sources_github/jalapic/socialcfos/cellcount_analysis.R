setwd("C:/Users/Cait/Dropbox/Research/Social Opportunity/june2016_6rem/csvs/cell counts")
setwd("C:/Users/caitl/Dropbox/Research/Social Opportunity/june2016_6rem/csvs/cell counts")
counts <- read.csv("cfos_counts_all.csv")

library(ggplot2)
library(lme4)
library(lmerTest)
library(MASS)
library(car)
library(rcompanion)
library(glmmADMB)


counts$countx <- as.integer(counts$countx)
counts$cohort <- as.factor(counts$cohort)
counts$removal <- as.factor(counts$removal)
counts$wins <- as.factor(counts$wins)
counts$losses <- as.integer(counts$losses)

BLA <- counts[counts$brain_region == "BLA", ]
DG <- counts[counts$brain_region == "DG", ]
CA3 <- counts[counts$brain_region == "CA3", ]
CA1 <- counts[counts$brain_region == "CA1", ]
IL <- counts[counts$brain_region == "IL", ]
PrL <- counts[counts$brain_region == "PrL", ]
BNST <- counts[counts$brain_region == "BNST", ]
LS <- counts[counts$brain_region == "LS", ]
meA <- counts[counts$brain_region == "meA", ]
mPOA <- counts[counts$brain_region == "mPOA", ]
AH <- counts[counts$brain_region == "AH", ]
VMH <- counts[counts$brain_region == "VMH", ]
vlPAG <- counts[counts$brain_region == "vlPAG", ]
dlPAG <- counts[counts$brain_region == "dlPAG", ]
PMd <- counts[counts$brain_region == "PMd", ]
PMv <- counts[counts$brain_region == "PMv", ]
RC <- counts[counts$brain_region == "RC", ]
ARC <- counts[counts$brain_region == "ARC", ]
CeA <- counts[counts$brain_region == "CeA", ]
LH <- counts[counts$brain_region == "LH", ]
Aud <- counts[counts$brain_region == "Aud", ]
Cing <- counts[counts$brain_region == "Cing", ]
CortA <- counts[counts$brain_region == "CortA", ]
Pir <- counts[counts$brain_region == "Pir", ]
Vis <- counts[counts$brain_region == "Vis", ]



### Anterior cortical amygdala

ACAxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins) + 
                     (1|losses), data = CortA)
summary(ACAxx)
gof(ACAxx)


### anterior hypothalamus

AHxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins) + 
                    (1|losses), data = AH)
summary(AHxx)
gof(AHxx)

### arcuate nucleus

ARCxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins) + 
                     (1|losses), data = ARC)
summary(ARCxx)
gof(ARCxx)


### auditory cortex

Audxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1|losses), data = Aud)
summary(Audxx)
gof(Audxx)

## basolateral amygdala 

BLAxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1|losses), data = BLA)
summary(BLAxx)
gof(BLAxx)


## BNST

BNSTxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                    + (1| losses), data = BNST)
summary(BNSTxx)
gof(BNSTxx)


### CA1
CA1xx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                    + (1| losses), data = CA1)
summary(CA1xx)
gof(CA1xx)

### CA3

CA3xx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1| losses), data = CA3)
summary(CA3xx)
gof(CA3xx)

### CeA 

CeAxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1| losses), data = CeA)
summary(CeAxx)
gof(CeAxx)


### Cing


Cingxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1| losses), data = Cing)
summary(Cingxx)
gof(Cingxx)


## DG


DGxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1| losses), data = DG)
summary(DGxx)
gof(DGxx)

### dlPAG


dlPAGxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1| losses), data = dlPAG)
summary(dlPAGxx)
gof(dlPAGxx)

## LH


LHxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1| losses), data = LH)
summary(LHxx)
gof(LHxx)


### LS


LSxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1| losses), data = LS)
summary(LSxx)
gof(LSxx)

## meA


meAxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1| losses), data = meA)
summary(meAxx)
gof(meAxx)


### mPOA

mPOAxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1| losses), data = mPOA)
summary(mPOAxx)
gof(mPOAxx)


### IL 

ILxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                  + (1|losses), data = IL)
summary(ILxx)
gof(ILxx)

## Pir


Pirxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                  + (1|losses), data = Pir)
summary(Pirxx)
gof(Pirxx)

### PrL

PrLxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                  + (1|losses), data = PrL)
summary(PrLxx)
gof(PrLxx)

### Pmd

PMdxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1|losses), data = PMd)
summary(PMdxx)
gof(PMdxx)

### Pmv

PMvxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1|losses), data = PMv)
summary(PMvxx)
gof(PMvxx)


### RC

RCxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1|losses), data = RC)
summary(RCxx)
gof(RCxx)

### Vis

Visxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1|losses), data = Vis)
summary(Visxx)
gof(Visxx)

### vlPAG

vlPAGxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                   + (1|losses), data = vlPAG)
summary(vlPAGxx)
gof(vlPAGxx)

## VMH

VMHxx  <- glmer.nb(countx ~ status + (1|cohort) + (1|removal) + (1|side) + (1|wins)
                     + (1|losses), data = VMH)
summary(VMHxx)
gof(VMHxx)

