library(readxl)
cm.name <- read_excel('communities.name.xlsx', col_names = F, sheet = 'Sheet2')
cm.names <- c()
for (i in 1:nrow(cm.name)){
  cm.names[i] <- cm.name[i,1]
}
attnames <- c()
for (i in 1:nrow(cm.name)){
  attnames[i] <- cm.names[[i]]
}
s<-sapply(attnames, strsplit, split=' ')
for (i in 1:length(attnames)){
  attnames[i] <- s[[i]][2]
}
cm.data<- read.csv('CommViolPredUnnormalizedData.txt', header=F, na.strings = '?')
colnames(cm.data)<- attnames
colnames(cm.data)[21]<- 'pctWRentInt'

# MISSING DATA TABLE BUILDING
miss.data<-c()
for (i in 1:ncol(cm.data)){
  if (any(is.na(cm.data[,i]))){
    miss.data[i]<- 'YES'
  } else{
    miss.data[i]<- 'NO'
  }
}
miss.data<- data.frame(colnames(cm.data), miss.data)
miss.data$misscount <- c(0)
for (i in 1:nrow(miss.data)){
  if (miss.data[i,2]=='YES'){
    miss.data[i,3]<-sum(is.na(cm.data[,i]))
  }
}

# FORMULA CONSTRUCTION
xnam <- c(colnames(cm.data)[6:103], colnames(cm.data)[121:123], colnames(cm.data[128]))
fmla <- as.formula(paste(colnames(cm.data)[147], " ~ ", paste(xnam, collapse= "+")))

# LM MODEL RUN
model <- lm(fmla, data=cm.data)
print(summary(model)$r.squared)

# SUMMARY EXTRACT FROM THE LM.MODEL
sum.model<- summary(model)
mc1<-sum.model$coefficients
mc2<- model$coefficients
p.vals<- as.data.frame(mc1[,c(1,4)])
colnames(p.vals)<- c("Coefficients","P.values")
sig.p.vals <- p.vals[order(p.vals$`P.values`, decreasing = F),]
sig.p.vals <- subset(sig.p.vals, sig.p.vals$`P.values`<0.05)
sig.p.vals$FactorNames<- row.names(sig.p.vals)
sig.p.vals$LogWorth<- -log10(sig.p.vals$P.values)
a<- anova(model)
x<- model$residuals
x<- data.frame(x)
colnames(x)<- c("Residuals")

#PLOTTING
#+++++++++++++++++
library(ggplot2)
pl1<-ggplot(data=x, aes(Residuals))+ geom_histogram(bins = 100, fill='black', alpha=0.5)+ labs(x='Residuals', y='Frequency', title = 'Residual Plot')
print(pl1)
#write.csv(sig.p.vals, file="CommunitySigPFactors2.csv")
