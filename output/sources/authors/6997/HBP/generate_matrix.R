generate_matrix<-function(all_hic_file,all_bed_file,outputpdf=FALSE,matrix_dir="dm3",chrom_file="chrom_dm3.sizes")
{
  all_bed_data=read.table(file=all_bed_file, fill=TRUE, stringsAsFactors=FALSE)
  all_hic_data=read.table(file=all_hic_file,fill=TRUE, stringsAsFactors=FALSE)
  chrom_info=read.table(chrom_file,fill=TRUE, stringsAsFactors=FALSE)
  chr_num=dim(chrom_info)[1]
  inter_interaction=NULL
  if(file.exists(matrix_dir)==FALSE)
  {
    dir.create(matrix_dir)
  }
  for(tttt in 1:chr_num)
  {
    chrname=chrom_info[tttt,1]
    chr_bed=NULL
    chr_bed=rbind(chr_bed,all_bed_data[all_bed_data[,1]==chrname,])
    chr_max_length=dim(chr_bed)[1]
    chr_hic=NULL
    chr_hic=all_hic_data[all_hic_data[,1]>=chr_bed[1,4],]
    chr_hic=chr_hic[chr_hic[,1]<=chr_bed[chr_max_length,4],]
    inter_interaction=rbind(inter_interaction,chr_hic[chr_hic[,2]<chr_bed[1,4],])
    inter_interaction=rbind(inter_interaction,chr_hic[chr_hic[,2]>chr_bed[chr_max_length,4],])
    chr_hic=chr_hic[chr_hic[,2]>=chr_bed[1,4],]
    chr_hic=chr_hic[chr_hic[,2]<=chr_bed[chr_max_length,4],]
    chr_hic_data=matrix(data=0, nrow = chr_max_length, ncol = chr_max_length)
    for(i in 1:dim(chr_hic)[1])
    {
      chr_hic_data[(chr_hic[i,1]-chr_bed[1,4]),(chr_hic[i,2]-chr_bed[1,4])]=chr_hic[i,3]
      chr_hic_data[(chr_hic[i,2]-chr_bed[1,4]),(chr_hic[i,1]-chr_bed[1,4])]=chr_hic[i,3]
      tmp_ys=i%%100000
      if(tmp_ys==0)
      {
        print(paste("hic data ",i," finish",sep = ""))
      }
    }
    tmpfilename=paste(matrix_dir,"/",chrname,".matrix",sep="")
    write.table(chr_hic_data,tmpfilename,quote=FALSE,sep="\t",row.names=FALSE,col.names=FALSE)

    if((outputpdf==TRUE)||(outputpdf=="TRUE")||(outputpdf=="true"))
    {
      pdf(paste(matrix_dir,"/",chrname,"_heatmap.pdf",sep=""))

    }else
    {
      jpeg(paste(matrix_dir,"/",chrname,"_heatmap.jpeg",sep=""),width=1000,height=1000,quality = 100)
    }
    hm_dim=dim(chr_hic_data)[1]
    hm_mean=mean(chr_hic_data)
    for(n in 1:hm_dim)
    {

      for(nn in 1:hm_dim)
      {
        if(chr_hic_data[n,nn]>10*hm_mean)
          chr_hic_data[n,nn]=10*hm_mean
      }
    }
    m_color=heat.colors(100)
    n_color=m_color
    for(i in 1:100)
    {
      m_color[i]=n_color[101-i]
    }
    tmphm=levelplot(chr_hic_data,col.regions=m_color,xlim=c(0,hm_dim),ylim=c(0,hm_dim),xlab="chrom",ylab="chrom")
    plot(tmphm)
    dev.off()


    rm(chr_hic_data)
    gc()
    mt=date()
    print(paste(mt," ",chrname," finish!",sep=""))
  }
  tmpfilename=paste(matrix_dir,"/inter_chrom.iam",sep="")
  write.table(inter_interaction,tmpfilename,quote=FALSE,sep="\t",row.names=FALSE,col.names=FALSE)
}
