Process_norm<-function(nbchip,nbarray,chip1_array1_Int1,chip1_array1_Int2,chip1_array2_Int1,chip1_array2_Int2,chip1_array3_Int1,chip1_array3_Int2,chip2_array1_Int1,chip2_array1_Int2,chip2_array2_Int1,chip2_array2_Int2,chip2_array3_Int1,chip2_array3_Int2,fileOUT,fileOUTtext,fileOUTdata,graph,Norm,MeanDyeswap,name.chr,sep.read="\t")
{
    path = attr(as.environment(match("package:TAHMMAnnot",search())),"path")

system(paste("perl ",path,"/exec/Pre-process_normalisation.pl ",nbchip," ",nbarray," ",fileOUT," ",path," ",chip1_array1_Int1," ",chip1_array1_Int2," ",chip1_array2_Int1," ",chip1_array2_Int2," ",chip1_array3_Int1," ",chip1_array3_Int2," ",chip2_array1_Int1," ",chip2_array1_Int2," ",chip2_array2_Int1," ",chip2_array2_Int2," ",chip2_array3_Int1," ",chip2_array3_Int2,sep=""),wait=TRUE)

print("Pre-process normalization OK")
cat("\n")


##Normalisation##

if((nbchip == 1 & nbarray == 1) | Norm==FALSE)
{
  data = read.table(fileOUT,header=TRUE)
  
  ID = data$ID
  CHIP = data$CHIP
  ARRAY = data$ARRAY
  DYE = data$DYE
  TREATMENT = data$TREATMENT
  

  VALUE = log2(data$VALUE);
  CHIP = CHIP[order(ID)];
  ARRAY = ARRAY[order(ID)];
  DYE = DYE[order(ID)];
  TREATMENT = TREATMENT[order(ID)];
  VALUE = VALUE[order(ID)];
  ID = sort(ID);

  NORM = data.frame(ID,CHIP,ARRAY,DYE,TREATMENT,VALUE);
  write.table(NORM,file=fileOUTdata,sep="\t",row.names=FALSE);


  detach(data)
  print("No ANOVA Normalization")
  cat("\n")


}else{
  Normalisation_ANOVA(fileIN=fileOUT,fileOUTtext=fileOUTtext,fileOUTdata=fileOUTdata,graph=graph)


print("ANOVA Normalization OK")
cat("\n")
}


system(paste("perl ",path,"/exec/Faire_fic_apresNorm_avant_ChIPmix.pl ",fileOUTdata," ",MeanDyeswap,sep=""),wait=TRUE)

print("files by chr with 3 columns OK")
cat("\n")
if(MeanDyeswap == TRUE)
{
	print("files averaged on the dye swap OK")
	cat("\n")	
}

########Ordonner les fichiers 3 colonnes###########


nbchr = length(name.chr)


for (chr in 1:nbchr)
{
	if(MeanDyeswap == TRUE)
	{
		fileINChIPmix = paste("MoyDye_",name.chr[chr],".txt",sep="")
		Sort_file(fileIN=fileINChIPmix,fileOUTdata=fileINChIPmix)

	}
	else
	{
		for(puce in 1:nbchip)
		{
			fileINChIPmix = paste("Puce",puce,"_",name.chr[chr],".txt",sep="")
			Sort_file(fileIN=fileINChIPmix,fileOUTdata=fileINChIPmix)
	}
}
}


print("files by chr with 3 columns ordered OK")
cat("\n")


}
