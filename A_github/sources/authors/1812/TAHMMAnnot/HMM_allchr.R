HMM_allchr = function(name.chr=c("CHR01","CHR02","CHR03","CHR04","CHR05"),color=c("navajowhite","grey","black","green","red"),beginfileIN="MoyDye",var1="IS1",var2="IS2",K=4, eps=10e-6, beginfile.RData="Param",beginfile.graph="Graph",file.flagdb="Fichier_flagdb.txt",beginfile.OUT="Output",beginfile.INIT="Graph_init",beginfile.param="Fichier_Param",threshold=FALSE,s=0.7,random.init=FALSE,a.inf=60,a.sup=75,b.inf=5,b.sup=20,PtInit=8,theta=0.4,int.max=8,max.iter=1000,header=TRUE,sep="\t", ...)
{

    for (n in 1:length(name.chr))
    {
	res=list()

        fileIN=paste(beginfileIN,"_",name.chr[n],".txt",sep="")
	file.RData=paste(beginfile.RData,"_",name.chr[n],".RData",sep="")
	file.graph=paste(beginfile.graph,"_",name.chr[n],".png",sep="")
	file.OUT=paste(beginfile.OUT,"_",name.chr[n],".txt",sep="")
 	file.INIT = paste(beginfile.INIT,"_",name.chr[n],".png",sep="")
	file.param = paste(beginfile.param,"_",name.chr[n],".txt",sep="")

        res[[n]] = HMM(color,fileIN,var1,var2,K,eps,file.RData,file.graph,file.flagdb,file.OUT,file.INIT,file.param,threshold,s,random.init,a.inf,a.sup,b.inf,b.sup,PtInit,theta,int.max,max.iter,header,sep, ...)
	cat(paste("Analysis ",name.chr[n]," OK \n",sep=""))
    }
    invisible(res)

}

