
setwd("/home/bst/student/hiparker/HeadNeck")
library("ProjectTemplate")
load.project()

# run sva on the database (will be used later in fsva) #
mod<-model.matrix(~as.factor(info.chung$HPV.Stat))
sva.frma<-sva(frma.chung,mod)
sva.combat.frma<-sva(combat.frma.chung,mod)

fsva.sva.frma <- fsva(dbdat=frma.chung, mod=mod, sv=sva.frma, newdat=frma.chung.naHPV, method="exact")	
fsva.sva.combat.frma <- fsva(dbdat=combat.frma.chung, mod=mod, sv=sva.combat.frma, newdat=frma.chung.naHPV, method="exact")	

# no batch correction
fit <- pamr.train(list(x=frma.chung,y=as.factor(info.chung$HPV.Stat)))
pred.none <- pamr.predict(fit,frma.chung.naHPV,threshold=2)

# combat on database only
fit <- pamr.train(list(x=combat.frma.chung,y=as.factor(info.chung$HPV.Stat)))
pred.combat <- pamr.predict(fit,frma.chung.naHPV,threshold=2)

# combat + fsva correction
fit <- pamr.train(list(x=combat.frma.chung,y=as.factor(info.chung$HPV.Stat)))
pred.combat.fsva <- pamr.predict(fit,fsva.sva.frma$new,threshold=2)

# sva on database only
fit <- pamr.train(list(x=fsva.sva.frma$db,y=as.factor(info.chung$HPV.Stat)))
pred.sva <- pamr.predict(fit,frma.chung.naHPV,threshold=2)

# sva + fsva correction
fit <- pamr.train(list(x=fsva.sva.frma$db,y=as.factor(info.chung$HPV.Stat)))
pred.sva.fsva <- pamr.predict(fit,fsva.sva.frma$new,threshold=2)

# sva + combat on database only
fit <- pamr.train(list(x=fsva.sva.combat.frma$db,y=as.factor(info.chung$HPV.Stat)))
pred.sva.combat <- pamr.predict(fit,frma.chung.naHPV,threshold=2)

# sva + combat + fsva correction
fit <- pamr.train(list(x=fsva.sva.combat.frma$db,y=as.factor(info.chung$HPV.Stat)))
pred.sva.combat.fsva <- pamr.predict(fit,fsva.sva.combat.frma$new,threshold=2)


predictions_nofSVA<-cbind(as.character(pred.none), as.character(pred.combat), as.character(pred.sva),as.character(pred.sva.combat))
rownames(predictions_nofSVA)<-info.chung.naHPV$Affy.Microarray
colnames(predictions_nofSVA)<-c("None","ComBat","SVA","ComBat+SVA")

predictions_fSVA<-cbind(as.character(pred.none), as.character(pred.combat.fsva), as.character(pred.sva.fsva),as.character(pred.sva.combat.fsva))
rownames(predictions_fSVA)<-info.chung.naHPV$Affy.Microarray
colnames(predictions_fSVA)<-c("None","ComBat+fSVA","SVA+fSVA","ComBat+SVA+fSVA")

ProjectTemplate::cache("predictions_nofSVA")
ProjectTemplate::cache("predictions_fSVA")

# in markdown document, write 
# print(xtable(predictions), type='html')


# p16 levels in predicted samples
yp1<-frma.chung.naHPV[temp[2], predictions_fSVA[,4]=="Pos"]
yp2<-frma.chung.naHPV[temp[3], predictions_fSVA[,4]=="Pos"]
yp3<-frma.chung.naHPV[temp[4], predictions_fSVA[,4]=="Pos"]
yp<-c(yp1,yp2,yp3)
xp<-c(1,1,1,2,2,2,3,3,3)

yn1<-frma.chung.naHPV[temp[2], predictions_fSVA[,4]=="Neg"]
yn2<-frma.chung.naHPV[temp[3], predictions_fSVA[,4]=="Neg"]
yn3<-frma.chung.naHPV[temp[4], predictions_fSVA[,4]=="Neg"]
yn<-c(yn1,yn2,yn3)
xn<-c(1,1,1,1,2,2,2,2,3,3,3,3)

cols <- brewer.pal(3, "Dark2")

setwd("C:/Users/Hilary/GitHub/HeadNeck/doc")
png(file="predictedp16.png")
plot(xp,yp,col=cols[2],pch=19,ylim=c(3,8.75),xaxt='n',xlab=" ",ylab="Expression Level",main="p16 Expression Levels in unlabeled samples")
points(xn,yn,pch=19,col=cols[3])
legend("topright",c("Predicted HPV Positive","Predicted HPV Negative"),col=cols[2:3],pch=19)
axis(side=1, at = 1:3, 
		 labels=c("Probe 1","Probe 2","Probe 3"), lwd=2, cex.axis=1.1)
dev.off()






## look at boxplots after fSVA correction, before fSVA correction
setwd("C:/Users/Hilary/GitHub/HeadNeck/graphs")

png(file="uncorrectednewsamps1.png")
manyboxplot(cbind(frma.chung,frma.chung.naHPV),dotcol=cols[1],linecol=cols[2:4],vlines=86.5,main="Uncorrected database, uncorrected samples")
dev.off()


png(file="uncorrectednewsamps2.png")
manyboxplot(cbind(sva.frma.chung,frma.chung.naHPV),dotcol=cols[1],linecol=cols[2:4],vlines=86.5,main="SVA corrected database, uncorrected samples")
dev.off()

manyboxplot(cbind(sva.frma.chung,fsva.sva.frma$new),dotcol=cols[1],linecol=cols[2:4],vlines=86.5,main="SVA corrected database, fSVA corrected samples")


png(file="correctednewsamps.png")
manyboxplot(cbind(sva.combat.frma.chung,fsva.sva.combat.frma$new),dotcol=cols[1],linecol=cols[2:4],vlines=86.5,main="SVA + ComBat corrected database, fSVA corrected samples")
dev.off()
