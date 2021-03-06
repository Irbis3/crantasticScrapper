if_distribution_analysis<-function(all_hic_file,all_bed_file,bedFile,inter_chromfile=NULL,groupNum=50,random_analysis=TRUE,threshold_percent=0.005,if_bin_number=20,outputpdf=FALSE,matrix_dir="dm3",resolution=5,chrom_file="chrom_dm3.sizes",slide_window=FALSE)
{
  all_bed_data=read.table(file=all_bed_file, fill=TRUE, stringsAsFactors=FALSE)
  all_hic_data=read.table(file=all_hic_file,fill=TRUE, stringsAsFactors=FALSE)
  colnames(all_hic_data)=c("loc1","loc2","value")
  colnames(all_bed_data)=c("chrom","start","end","id")
  loc1="loc1"
  loc2="loc2"
  chrom_info=read.table(chrom_file,fill=TRUE, stringsAsFactors=FALSE)
  chr_num=dim(chrom_info)[1]
  m_bed_data=read.table(file=bedFile,fill=TRUE, stringsAsFactors=FALSE)
  if(is.character(m_bed_data[,2])==TRUE)
  {
    m_bed_data=read.table(file=bedFile,fill=TRUE, stringsAsFactors=FALSE,skip=1)

  }

  if(is.null(inter_chromfile))
  {
    inter_interaction=read.table(file=paste(matrix_dir,"/inter_chrom.iam",sep=""), fill=TRUE, stringsAsFactors=FALSE)
  }else
  {
    inter_interaction=read.table(file=inter_chromfile, fill=TRUE, stringsAsFactors=FALSE)
  }
  order_hic_data<-all_hic_data[with(all_hic_data, order(value,decreasing = TRUE)), ]
  sum_counts=sum(order_hic_data[,3])
  th_1=mean(inter_interaction[,3])
  if(threshold_percent==0)
  {
    th_2=order_hic_data[1,3]
  }else
  {
    th_2=order_hic_data[ceiling(dim(order_hic_data)[1]*threshold_percent),3]
  }
  if_bin_size=(th_2-th_1)/if_bin_number
  order_hic_data=NULL
  rm(order_hic_data)
  gc()
  all_hic_data=all_hic_data[which(all_hic_data[,3]>=th_1),]
  all_hic_data=all_hic_data[which(all_hic_data[,3]<=th_2),]
  all_hic_data=all_hic_data[which(all_hic_data[,1]!=all_hic_data[,2]),]

  m_bed_loc=NULL
  for(nn in 1:dim(m_bed_data)[1])
  {
    tmp_bed_loc=all_bed_data[which(all_bed_data[,1]==m_bed_data[nn,1]),]
    tmp_bed_loc=tmp_bed_loc[which(tmp_bed_loc[,2]>(m_bed_data[nn,2]-resolution*1000)),]
    tmp_bed_loc=tmp_bed_loc[which(tmp_bed_loc[,3]<(m_bed_data[nn,3]+resolution*1000)),]
    m_bed_loc=rbind(m_bed_loc,tmp_bed_loc)
  }
  m_bed_loc=m_bed_loc[!duplicated(m_bed_loc),]

  all_coverage=matrix(data=0, nrow = chr_num, ncol = if_bin_number)
  all_actionnum=matrix(data=0,nrow=chr_num,ncol=if_bin_number)
  all_bb_ia=matrix(data=0,nrow=chr_num,ncol = if_bin_number)

  random_coverage=array(0,c(chr_num,if_bin_number,groupNum))
  random_bb_ia=array(0,c(chr_num,if_bin_number,groupNum))
  for(tttt in 1:chr_num)
  {
    chrname=chrom_info[tttt,1]
    chr_bed=NULL
    chr_bed=rbind(chr_bed,all_bed_data[all_bed_data[,1]==chrname,])
    chr_max_length=dim(chr_bed)[1]
    chr_hic=NULL
    chr_hic=all_hic_data[all_hic_data[,1]>=chr_bed[1,4],]
    chr_hic=chr_hic[chr_hic[,1]<=chr_bed[chr_max_length,4],]
    chr_hic=chr_hic[chr_hic[,2]>=chr_bed[1,4],]
    chr_hic=chr_hic[chr_hic[,2]<=chr_bed[chr_max_length,4],]
    order_chr_hic<-chr_hic[with(chr_hic, order(value,decreasing = TRUE)), ]
    chr_bed_loc=as.vector(m_bed_loc[which(m_bed_loc[,1]==chrname),4])
    for(mm in 1:if_bin_number)
    {
      if_bin_start=th_1+(mm-1)*if_bin_size
      if_bin_end=th_1+mm*if_bin_size
      if_bin_hic=chr_hic[which(chr_hic[,3]>=if_bin_start),]
      if_bin_hic=if_bin_hic[which(if_bin_hic[,3]<if_bin_end),]
      hic_overlap=rbind(subset(if_bin_hic,loc1%in%chr_bed_loc),subset(if_bin_hic,loc2%in%chr_bed_loc))
      hic_overlap=hic_overlap[!duplicated(hic_overlap),]
      hic_bb_ia_1=subset(if_bin_hic,loc1%in%chr_bed_loc)
      hic_bb_ia_1=subset(hic_bb_ia_1,loc2%in%chr_bed_loc)
      hic_bb_ia_2=subset(if_bin_hic,loc2%in%chr_bed_loc)
      hic_bb_ia_2=subset(hic_bb_ia_2,loc1%in%chr_bed_loc)
      hic_bb_ia=rbind(hic_bb_ia_1,hic_bb_ia_2)
      hic_bb_ia=hic_bb_ia[!duplicated(hic_bb_ia),]
      all_actionnum[tttt,mm]=dim(if_bin_hic)[1]
      all_coverage[tttt,mm]=dim(hic_overlap)[1]
      all_bb_ia[tttt,mm]=dim(hic_bb_ia)[1]
      if((random_analysis==TRUE)||(random_analysis=="TRUE")||(random_analysis=="true"))
      {
        min_loc=min(chr_bed[,4])
        max_loc=max(chr_bed[,4])
        loc_number=length(chr_bed_loc)
        for(bb in 1:groupNum)
        {
          random_site=sample(min_loc:max_loc,size=loc_number)
          random_site=sort(random_site)
          random_overlap=rbind(subset(if_bin_hic,loc1%in%random_site),subset(if_bin_hic,loc2%in%random_site))
          random_overlap=random_overlap[!duplicated(random_overlap),]
          n_bb_ia_1=subset(if_bin_hic,loc1%in%random_site)
          n_bb_ia_1=subset(n_bb_ia_1,loc2%in%random_site)
          n_bb_ia_2=subset(if_bin_hic,loc2%in%random_site)
          n_bb_ia_2=subset(n_bb_ia_2,loc1%in%random_site)
          n_bb_ia=rbind(n_bb_ia_1,n_bb_ia_2)
          n_bb_ia=n_bb_ia[!duplicated(n_bb_ia),]
          random_coverage[tttt,mm,bb]=dim(random_overlap)[1]
          random_bb_ia[tttt,mm,bb]=dim(n_bb_ia)[1]
        }
      }
    }
  }

  coverage_percent=matrix(data=0,nrow=chr_num,ncol=if_bin_number)
  bb_ia_percent=matrix(data=0,nrow=chr_num,ncol=if_bin_number)
  random_cp=array(0,c(chr_num,if_bin_number,groupNum))
  random_bb=array(0,c(chr_num,if_bin_number,groupNum))
  sum_cp=matrix(data=0,nrow=1,ncol=if_bin_number)
  random_sum_cp=matrix(data=0,nrow=groupNum,ncol=if_bin_number)
  for(i1 in 1:if_bin_number)
  {
    if(sum(all_actionnum[,i1])!=0)
    {
      sum_cp[1,i1]=(sum(all_coverage[,i1]))/(sum(all_actionnum[,i1]))
    }
    if(sum(all_actionnum[,i1])==0)
    {
      sum_cp[1,i1]=0
    }
    if((random_analysis==TRUE)||(random_analysis=="TRUE")||(random_analysis=="true"))
    {
      for(i2 in 1:groupNum)
      {
        if(sum(all_actionnum[,i1])!=0)
        {
          random_sum_cp[i2,i1]=(sum(random_coverage[,i1,i2]))/(sum(all_actionnum[,i1]))
        }
        if(sum(all_actionnum[,i1])==0)
        {
          random_sum_cp[i2,i1]=0
        }
      }
    }
  }

  for(i1 in 1:chr_num)
  {
    for(i2 in 1:if_bin_number)
    {
      if(all_actionnum[i1,i2]!=0)
      {
        coverage_percent[i1,i2]=all_coverage[i1,i2]/all_actionnum[i1,i2]
        bb_ia_percent[i1,i2]=all_bb_ia[i1,i2]/all_actionnum[i1,i2]
      }
      if(all_actionnum[i1,i2]==0)
      {
        coverage_percent[i1,i2]=0
        bb_ia_percent[i1,i2]=0
      }
      #coverage_percent[i1,i2]=all_coverage[i1,i2]/all_actionnum[i1,i2]
      if((random_analysis==TRUE)||(random_analysis=="TRUE")||(random_analysis=="true"))
      {
        for(i3 in 1:groupNum)
        {
          if(all_actionnum[i1,i2]!=0)
          {
            random_cp[i1,i2,i3]=random_coverage[i1,i2,i3]/all_actionnum[i1,i2]
            random_bb[i1,i2,i3]=random_bb_ia[i1,i2,i3]/all_actionnum[i1,i2]
          }
          if(all_actionnum[i1,i2]==0)
          {
            random_cp[i1,i2,i3]=0
            random_bb[i1,i2,i3]=0
          }
        }
      }
    }
  }


  ttest_result=array(0,c(chr_num,if_bin_number,5))
  bb_ttest_result=array(0,c(chr_num,if_bin_number,5))
  sum_ttest=matrix(data=0,nrow = 5,ncol=if_bin_number)
  for(i1 in 1:if_bin_number)
  {
    tmpttest=t.test(random_sum_cp[,i1],mu=sum_cp[1,i1])
    sum_ttest[1,i1]=tmpttest$conf.int[1]
    sum_ttest[2,i1]=tmpttest$conf.int[2]
    sum_ttest[3,i1]=tmpttest$estimate
    sum_ttest[4,i1]=sum_cp[1,i1]
    sum_ttest[5,i1]=tmpttest$p.value
  }
  for(i1 in 1:chr_num)
  {
    for(i2 in 1:if_bin_number)
    {
      tmpttest=t.test(random_cp[i1,i2,],mu=coverage_percent[i1,i2])
      ttest_result[i1,i2,1]=tmpttest$conf.int[1]
      ttest_result[i1,i2,2]=tmpttest$conf.int[2]
      ttest_result[i1,i2,3]=tmpttest$estimate
      ttest_result[i1,i2,4]=coverage_percent[i1,i2]
      ttest_result[i1,i2,5]=tmpttest$p.value

      tmpttest=t.test(random_bb[i1,i2,],mu=bb_ia_percent[i1,i2])
      bb_ttest_result[i1,i2,1]=tmpttest$conf.int[1]
      bb_ttest_result[i1,i2,2]=tmpttest$conf.int[2]
      bb_ttest_result[i1,i2,3]=tmpttest$estimate
      bb_ttest_result[i1,i2,4]=coverage_percent[i1,i2]
      bb_ttest_result[i1,i2,5]=tmpttest$p.value
    }
  }

  m_col=c("#e6f5c9","#ffffbf")

  if((slide_window==TRUE)||(slide_window=="TRUE")||(slide_window=="true"))
  {
    for(i1 in 1:chr_num)
    {
      chrname=chrom_info[i1,1]
      if((outputpdf==TRUE)||(outputpdf=="TRUE")||(outputpdf=="true"))
      {
        pdf(paste(matrix_dir,"/",chrname,"_if_dis_slide.pdf",sep=""),width = 8,height = 8)

      }else
      {
        jpeg(paste(matrix_dir,"/",chrname,"_if_dis_slide.jpeg",sep=""),width=1000,height=1000,quality = 100)
      }
      boxplot(t(random_cp[i1,,]),col=m_col,xlab="interaction frequency distribution")
      #lines(ttest_result[i1,,4],lwd=3,col="red",lty=4)
      m_ly=numeric(if_bin_number)
      for(m_li in 1:(if_bin_number-2))
      {
        m_ly[m_li+1]=sum(ttest_result[i1,m_li:(m_li+2),4])/3
      }
      m_ly[1]=sum(ttest_result[i1,1:2,4])/2
      m_ly[if_bin_number]=sum(ttest_result[i1,(if_bin_number-1):if_bin_number,4])/2
      lines(m_ly,lwd=3,col="red",lty=4)
      dev.off()
    }
    if((outputpdf==TRUE)||(outputpdf=="TRUE")||(outputpdf=="true"))
    {
      pdf(paste(matrix_dir,"/all_chrom_if_dis_slide.pdf",sep=""),width = 8,height = 8)

    }else
    {
      jpeg(paste(matrix_dir,"/all_chrom_if_dis_slide.jpeg",sep=""),width=1000,height=1000,quality = 100)
    }

    boxplot(random_sum_cp,col=m_col,xlab="interaction frequency distribution")
    #lines(sum_ttest[4,],lwd=3,col="red",lty=4)
    sum_ly=numeric(if_bin_number)
    for(sum_li in 1:(if_bin_number-2))
    {
      sum_ly[sum_li+1]=sum(sum_ttest[4,sum_li:(sum_li+2)])/3
    }
    sum_ly[1]=sum(sum_ttest[4,1:2])/2
    sum_ly[if_bin_number]=sum(sum_ttest[4,(if_bin_number-1):if_bin_number])/2
    lines(sum_ly,lwd=3,col="red",lty=4)
    dev.off()
  }else
  {
    for(i1 in 1:chr_num)
    {
      chrname=chrom_info[i1,1]
      if((outputpdf==TRUE)||(outputpdf=="TRUE")||(outputpdf=="true"))
      {
        pdf(paste(matrix_dir,"/",chrname,"_if_dis.pdf",sep=""),width = 8,height = 8)

      }else
      {
        jpeg(paste(matrix_dir,"/",chrname,"_if_dis.jpeg",sep=""),width=1000,height=1000,quality = 100)
      }
      boxplot(t(random_cp[i1,,]),col=m_col,xlab="interaction frequency distribution")
      lines(ttest_result[i1,,4],lwd=3,col="red",lty=4)
      dev.off()
    }
    if((outputpdf==TRUE)||(outputpdf=="TRUE")||(outputpdf=="true"))
    {
      pdf(paste(matrix_dir,"/all_chrom_if_dis.pdf",sep=""),width = 8,height = 8)

    }else
    {
      jpeg(paste(matrix_dir,"/all_chrom_if_dis.jpeg",sep=""),width=1000,height=1000,quality = 100)
    }

    boxplot(random_sum_cp,col=m_col,xlab="interaction frequency distribution")
    lines(sum_ttest[4,],lwd=3,col="red",lty=4)
    dev.off()
  }


}
