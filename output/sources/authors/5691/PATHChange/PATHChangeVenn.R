########################################################################################################
########################################################################################################
####                                   PATHChange Package
########################################################################################################
########################################################################################################
###################################  PATHChangeVenn function  ##########################################
########################################################################################################

PATHChangeVenn <- function(p.value, p, writePDF, destDIR){
  Boot <- Fisher <- Wilc <- list()
  for (j in 1:length(p.value)){
    Boot[[j]] <- which(as.numeric(p.value[[j]][,"Bootstrap"])<=p|as.numeric(p.value[[j]][,"Bootstrap"])>=1-p)
    Fisher[[j]] <- which(as.numeric(p.value[[j]][,"Fisher"])<=p)
    Wilc[[j]] <- which(as.numeric(p.value[[j]][,"Wilcoxon"])<=p)
    grid.newpage()
    venn.plot <- paste("venn.plot", j, sep=".") 
    venn.plot.j<- draw.triple.venn(area1=length(Boot[[j]]),area2=length(Fisher[[j]]),area3=length(Wilc[[j]]), 
                                   n12 = length(grep(TRUE, (Boot[[j]]%in%Fisher[[j]]))), 
                                   n23 = length(grep(TRUE, (Fisher[[j]]%in%Wilc[[j]]))), 
                                   n13 = length(grep(TRUE, (Boot[[j]]%in%Wilc[[j]]))), 
                                   n123 = length(intersect(Boot[[j]],intersect(Fisher[[j]],Wilc[[j]]))), 
                                   category = c("Bootstrap", "Fisher", "Wilcoxon"), 
                                   lty = "blank",
                                   margin = 0.05,
                                   cex = 1.5,
                                   cat.cex = 1.5,
                                   fill = c("skyblue","pink1","mediumorchid"))
    if(writePDF==TRUE){pdf(paste0(destDIR,"/",(paste(paste("VennDiagram", j, sep="_") ,".pdf", sep=""))))}
    grid.draw(venn.plot.j);
    dev.off()
  }
  return(grid.draw(venn.plot.j))
}
