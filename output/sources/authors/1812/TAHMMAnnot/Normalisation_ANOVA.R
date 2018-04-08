#License:   This file is part of ChIPmix for TAG Project.
#
#    ChIPmix for TAG Project is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    ChIPmix for TAG Project is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with ChIPmix for TAG Project.  If not, see <http://www.gnu.org/licenses/>.


###############################################################################################
#Normalisation.R         08-10-08
#
#Aim :             Application of a normalisation by ANOVA to ChIP-chip data for one dye-swap
#                  to remove technical biases
#-----------------------------------------------------------------------------------------------
#Main function : - log-transformed intensities
#                - normalisation ANOVA
#-----------------------------------------------------------------------------------------------
#Arguments :     - fileIN = name of the input data file with 6 columns
#                  named ID, CHIP, ARRAY, DYE, TREATMENT, VALUE.
#                - fileOUTtext = name of the results file for estimation parameters and means squares
#                - fileOUTdata = name of the output data file with 6 columns (the first 5 columns are 
#                                the same as in input file and the column 6 is named VALUEnorm which
#                                is the intensity of the probe after normalisation)
#                - graph : If TRUE, print graphs before and after normalisation by chip, by array 
#                          and by treatment for a dye (default value = FALSE)
#-----------------------------------------------------------------------------------------------
#Output :        - 2 files: one for the parameters estimation of the normalisation function,
#                           one for the output data after normalisation.
#		 - graphs if the option graph=TRUE.
#------------------------------------------------------------------------------------------------
#Dependencies : 
#------------------------------------------------------------------------------------------------
#Authors : Caroline Bérard: caroline.berard@agroparistech.fr
#          Marie-Laure Martin-Magniette: marie_laure.martin@agroparistech.fr
#----------------------------------------------------------------------------------------------
#
#Date : 08/10/2008
#-----------------------------------------------------------------------------------------------
#License : GPL
#          (c) 2008 Institut National de la Recherche Agronomique
#-----------------------------------------------------------------------------------------------
#Warning : The intensities in the input file are not log-tranformed
################################################################################################

Normalisation_ANOVA = function (fileIN="Normalisation_data_Rep1.txt",fileOUTtext="Results_Normalisation_Rep1.txt",fileOUTdata="data_APRESnorm_Rep1.txt",graph=FALSE)
  {

data = read.table(fileIN,header=TRUE)

VALUE = log2(data$VALUE);
ID = data$ID
TREATMENT = data$TREATMENT

options(contrasts=c("contr.sum","contr.sum"))

fileIN2 = strsplit(fileIN,".txt")
fileOUTgraph1 = paste("Before_",fileIN2,sep="")
fileOUTgraph2 = paste("After_",fileIN2,sep="")

CHIP = as.factor(data$CHIP);
ARRAY = as.factor(data$ARRAY);
DYE = as.factor(data$DYE);

nbpuce = length(levels(CHIP))
nblame = length(levels(ARRAY))
nbdye = length(levels(DYE))

if(nbdye == 1)
{
  print("ERROR : nbdye=1")
  break;
}

if(nbpuce == 1 & nblame == 1)
{
  print("ERROR :  Useless to standardize because nbpuce=1 and nblame=1. Put option = FALSE ")
  break;
}

if(nbpuce > 1 & nblame > 1)
{
  fo = as.formula(VALUE ~ CHIP*ARRAY + CHIP*DYE)
}

if(nbpuce > 1 & nblame == 1)
{
  fo = as.formula(VALUE ~ CHIP*DYE)
}

if(nbpuce == 1 & nblame > 1)
{
  fo = as.formula(VALUE ~ ARRAY) #juste un effet lame ? 
}


sink(fileOUTtext);
print("Before normalization :");
cat("\n");
Results = aov(fo);
print(summary(Results));
print(Results$coefficients);
cat("\n");


#VALUEnorm = VALUE-effetpuce-effetlame-effetpucelame-effetdye-effetpucedye
#Or Ychapeau = Results$fitted = intercept+effetpuce+effetlame+effetpucelame+effetdye+effetpucedye
#Donc VALUEnorm = Y(=obs) - Ychapeau(=Results$fitted) + intercept
#ou bien VALUEnorm = Results$residuals + intercept

VALUEnorm = Results$residuals + Results$coefficients[1];



#Pour avoir les donnees dans le meme ordre en abscisses et en ordonnees
CHIP = CHIP[order(ID)];
ARRAY = ARRAY[order(ID)];
DYE = DYE[order(ID)];
TREATMENT = TREATMENT[order(ID)];
VALUE = VALUE[order(ID)];
VALUEnorm = VALUEnorm[order(ID)];
ID = sort(ID);


if(graph == TRUE & nbpuce>1)
{

#Graphiques avant normalisation

TREATMENT = as.factor(TREATMENT);
for(t in 1:length(levels(TREATMENT)))
  {
    for(p in 1:(nbpuce-1))
    {

	pdf(file=paste(fileOUTgraph1,"_puce",p,"vspuce",p+1,"_",levels(TREATMENT)[t],".pdf",sep=""));
	plot(VALUE[CHIP == p & TREATMENT == levels(TREATMENT)[t]],VALUE[CHIP == p+1 & TREATMENT == levels(TREATMENT)[t]],pch='.',xlim=c(6,16),ylim=c(6,16),xlab=paste("signal de la puce",p ," pour le traitement ",levels(TREATMENT)[t],sep=""),ylab=paste("signal de la puce",p+1," pour le traitement ",levels(TREATMENT)[t],sep=""));
	abline(0,1,col="red");
	dev.off()

	for (l in 1:(nblame))
	{
	   pdf(file=paste(fileOUTgraph1,"_puce",p,"vspuce",p+1,"_lame",l,"_",levels(TREATMENT)[t],".pdf",sep=""));
	   plot(VALUE[CHIP == p & ARRAY == l & TREATMENT == levels(TREATMENT)[t]],VALUE[CHIP == p+1 & ARRAY == l & TREATMENT == levels(TREATMENT)[t]],pch='.',xlim=c(6,16),ylim=c(6,16),xlab=paste("signal de la puce",p ," lame",l," pour le traitement ",levels(TREATMENT)[t],sep=""),ylab=paste("signal de la puce",p+1," lame",l," pour le traitement ",levels(TREATMENT)[t],sep=""));
	   abline(0,1,col="red");
	   dev.off()

	}
    }
 }

#Graphiques après normalisation


TREATMENT = as.factor(TREATMENT);
for(t in 1:length(levels(TREATMENT)))
  {
    for(p in 1:(nbpuce-1))
    {

	pdf(file=paste(fileOUTgraph2,"_puce",p,"vspuce",p+1,"_",levels(TREATMENT)[t],".pdf",sep=""));
	plot(VALUEnorm[CHIP == p & TREATMENT == levels(TREATMENT)[t]],VALUE[CHIP == p+1 & TREATMENT == levels(TREATMENT)[t]],pch='.',xlim=c(6,16),ylim=c(6,16),xlab=paste("signal de la puce",p ," pour le traitement ",levels(TREATMENT)[t],sep=""),ylab=paste("signal de la puce",p+1," pour le traitement ",levels(TREATMENT)[t],sep=""));
	abline(0,1,col="red");
	dev.off()

	for (l in 1:(nblame))
	{
	   pdf(file=paste(fileOUTgraph2,"_puce",p,"vspuce",p+1,"_lame",l,"_",levels(TREATMENT)[t],".pdf",sep=""));
	   plot(VALUEnorm[CHIP == p & ARRAY == l & TREATMENT == levels(TREATMENT)[t]],VALUE[CHIP == p+1 & ARRAY == l & TREATMENT == levels(TREATMENT)[t]],pch='.',xlim=c(6,16),ylim=c(6,16),xlab=paste("signal de la puce",p ," lame",l," pour le traitement ",levels(TREATMENT)[t],sep=""),ylab=paste("signal de la puce",p+1," lame",l," pour le traitement ",levels(TREATMENT)[t],sep=""));
	   abline(0,1,col="red");
	   dev.off()

	}
    }
 }

}

#Vérifier qu'une fois les données normalisées, les CM(carrés moyens = Sum of Square / ddl) sont très proches de 0

if(nbpuce > 1 & nblame > 1)
{
  fonorm = as.formula(VALUEnorm ~ CHIP*ARRAY + CHIP*DYE)
}

if(nbpuce > 1 & nblame == 1)
{
  fonorm = as.formula(VALUEnorm ~ CHIP*DYE)
}

if(nbpuce == 1 & nblame > 1)
{
  fonorm = as.formula(VALUEnorm ~ ARRAY) 
}

print("After normalization, the CM have to be near to 0 :");
cat("\n");
Results = aov(fonorm);
print(summary(Results));

#Ressortir un fichier avec les valeurs normalisées pour ensuite faire les analyses dessus

NORM = data.frame(ID,CHIP,ARRAY,DYE,TREATMENT,VALUEnorm);
write.table(NORM,file=fileOUTdata,sep="\t",row.names=FALSE);


detach(data)
sink()

}
