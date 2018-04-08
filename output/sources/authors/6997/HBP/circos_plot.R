circos_plot<-function(bedFile,wig_dir="dm3wig",matrix_dir="dm3_5k",bedWindow=0,outputpdf=FALSE,chrom="all",chrstart=0,chrend=0,resolution=5,circosLineWidth=0.01,if_threshold=0,circosLinecolor="ReadCounts",circosTrackWidth=40)
{
  #source("calbed.R")
  gc()
  matrix_name_dir=list.files(path=matrix_dir,full.names=F,pattern=".matrix")
  matrix_full_dir=list.files(path=matrix_dir,full.names=T,pattern=".matrix")
  all_wig_file=list.files(path=wig_dir,full.names=T,pattern=".wig")
  all_wig_name=list.files(path=wig_dir,full.names = FALSE,pattern = ".wig")
  all_wig_num=length(all_wig_file)
  m_bed=load_bed(bedFile)
  chrNum=length(matrix_name_dir)
  options(stringsAsFactors = FALSE);

  genomeFrame=data.frame("seg.name"=character(0),"seg.start"=numeric(0),"seg.end"=numeric(0),"the.v"=character(0),"NO"=character(0),stringsAsFactors=FALSE)
  genomeFrameNum=1
  print("generate genome frame")
  for (i in 1:chrNum)
  {
    chrCmap=read.table(file=matrix_full_dir[i], fill=TRUE, stringsAsFactors=FALSE)
    chrTotSize=dim(chrCmap)[1]
    tmpNum=regexpr(".matrix",matrix_name_dir[i])
    chrName=substr(matrix_name_dir[i],1,tmpNum-1)

    if(chrName==chrom)
    {
      if(chrend>0)
      {
        tmpCmapStart=abs(ceiling((chrstart/(resolution*1000))))
        tmpCmapEnd=abs(ceiling((chrend/(resolution*1000))))
        if(tmpCmapEnd<tmpCmapStart)
        {
          print("please input correct start and end number")
          break
        }
        if(tmpCmapEnd>chrTotSize)
        {
          tmpCmapEnd=chrTotSize
        }
        if(tmpCmapStart>chrTotSize)
        {
          tmpCmapStart=chrTotSize
        }
        chrCmap=chrCmap[tmpCmapStart:tmpCmapEnd,tmpCmapStart:tmpCmapEnd]
        chrTotSize=dim(chrCmap)[1]
      }
    }

    for (ii in 1:chrTotSize)
    {
      genomeFrame[genomeFrameNum,1]=chrName
      genomeFrame[genomeFrameNum,2]=resolution*1000*(ii-1)
      genomeFrame[genomeFrameNum,3]=resolution*1000*ii
      genomeFrame[genomeFrameNum,4]=NA
      genomeFrame[genomeFrameNum,5]=NA
      genomeFrameNum=genomeFrameNum+1
    }
    print(paste(chrName," frame generate finished!",sep = ""))


    rm(chrCmap)
    gc()
  }

  if(chrom=="all")
  {
    for (i in 1:chrNum)
    {

      tmpNum=regexpr(".matrix",matrix_name_dir[i])
      chrName=substr(matrix_name_dir[i],1,tmpNum-1)
      chrBed=choose_chr_bed(m_bed,chrName)
      chrBedNum=dim(chrBed)[1]
      if(chrBedNum>0)
      {
        chrCmap=read.table(file=matrix_full_dir[i], fill=TRUE, stringsAsFactors=FALSE)
        chrTotSize=dim(chrCmap)[1]
        tmpNum2=regexpr("r",chrName)
        chrNo=substr(chrName,tmpNum2+1,nchar(chrName))
        chrBedBin=check_bed_bin(chrBed,resolution*1000)
        chrBedMatrix=convert_bed_to_matrix(chrBed,resolution*1000,chrName,chrTotSize,bedWindow)
        print(paste("plot ",chrName,"circos picture",sep=""))

        chrCircosMapping=calculate_omiccircos_data(chrCmap,chrBedMatrix,chrBedBin,chrBed,chrName,chrCmap,if_threshold)
        if((dim(chrCircosMapping)[1])<5)
        {
          next;
        }
        chrCircosMapping[,1]=chrNo
        chrCircosMapping[,4]=chrNo
        chrCircosDb=segAnglePo(genomeFrame, seg=chrName)
        seg.num<-length(unique(genomeFrame[,1]))
        colors<-rainbow(seg.num, alpha=0.5)


        if((outputpdf==TRUE)||(outputpdf=="TRUE")||(outputpdf=="true"))
        {
          pdf(paste(matrix_dir,"/",chrName,"_circos.pdf",sep=""),width = 8,height = 8)

        }else
        {
          jpeg(paste(matrix_dir,"/",chrName,"_circos.jpeg",sep=""),width=1000,height=1000,quality = 100)
        }
        #pdf("tmp_circos.pdf",width = 8,height = 8)
        options(stringsAsFactors = FALSE);

        par(mar=c(2, 2, 2, 2));
        plot(c(1,800), c(1,800), type="n", axes=FALSE, xlab="", ylab="", main="");
        if(circosLinecolor=="rainbow")
        {
          colors<-rainbow(seg.num, alpha=0.5)
          circosLinecolor=colors
          circos(R=340, cir=chrCircosDb, W=40,  mapping=chrCircosMapping, type="link2", lwd=circosLineWidth,col=colors);
        }else if(circosLinecolor=="ReadCounts")
        {
          #colors<-c("#f7f4f9","#f7f4f9","#e7e1ef","#d4b9da","#c994c7","#df65b0","#e7298a","#ce1256","#980043","#67001f")
          colors<-c("#fff7f3","#fff7f3","#fde0dd","#fcc5c0","#fa9fb5","#f768a1","#dd3497","#b0017e","#7a0177","#49006a")
          #colors<-c("#fff7fb","#fff7fb","#ece7f2","#d0d1e6","#a6bddb","#74a9cf","#3690c0","#0570b0","#045a8d","#023858")
          #colors<-c("#ffffcc","#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#b00026")
          #colors<-c("#f7fbff","#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
          chrCircosMapping[,7]=as.numeric(chrCircosMapping[,7])
          chrCircosMapping<-chrCircosMapping[with(chrCircosMapping, order(read_count,decreasing = FALSE)), ]
          ccm_dim=dim(chrCircosMapping)[1]
          for(ccm in 1:10)
          {
            ccm_start=1+ceiling((ccm_dim/10)*(ccm-1))
            ccm_end=ceiling((ccm_dim/10)*ccm)
            circos(R=340, cir=chrCircosDb, W=40,  mapping=chrCircosMapping[ccm_start:ccm_end,], type="link2", lwd=circosLineWidth*ccm,col=colors[ccm]);
          }
        }else
        {
          circos(R=340, cir=chrCircosDb, W=40,  mapping=chrCircosMapping, type="link2", lwd=circosLineWidth,col=circosLinecolor);
        }

        all_wig_hm=NULL
        all_bed_wig_hm=NULL

        if(all_wig_num>0)
        {
          for(ww in 1:all_wig_num)
          {
            m_wig=load_wig(all_wig_file[ww],resolution*1000,chrName,chrTotSize,chrstart,chrend)


            chrBedWig=m_wig[which(chrBedMatrix[,1]==1),]
            chrBedWig[,2]=chrBedWig[,2]*resolution*1000
            m_wig[,2]=m_wig[,2]*resolution*1000

            m_wig_ttest=NULL
            if((length(chrBedWig))>0)
            {
              for(iii in 1:(dim(chrBedWig)[1]))
              {
                m_wig_ttest=rbind(m_wig_ttest,t.test(m_wig[,3],mu=chrBedWig[iii,3]))
              }
              m_wig_equal=which(m_wig_ttest[,3]>0.05)
              m_wig_mean=mean(m_wig[,3])
              m_wig_nequal=which(m_wig_ttest[,3]<=0.05)
              m_wig_more=m_wig_nequal[which(chrBedWig[m_wig_nequal[],3]>m_wig_mean)]
              m_wig_less=m_wig_nequal[which(chrBedWig[m_wig_nequal[],3]<=m_wig_mean)]
              m_wig_bed=chrBedWig
              m_wig_bed[,3]=0
              m_wig_bed[m_wig_more,3]=1
              m_wig_bed[m_wig_less,3]=-1

              if(is.null(all_bed_wig_hm))
              {
                all_bed_wig_hm=m_wig_bed
              }else
              {
                all_bed_wig_hm=cbind(all_bed_wig_hm,m_wig_bed[,3])
              }
              if(is.null(all_wig_hm))
              {
                all_wig_hm=m_wig
                all_wig_hm[,3]=(all_wig_hm[,3]-range(all_wig_hm[,3])[1])/(range(all_wig_hm[,3])[2]-range(all_wig_hm[,3])[1])
                tmpname=colnames(all_wig_hm)
                colnames(all_wig_hm)=c(tmpname[1:2],all_wig_name[ww])
              }else
              {
                tmpname=colnames(all_wig_hm)
                all_wig_hm=cbind(all_wig_hm,((m_wig[,3]-range(m_wig[,3])[1])/(range(m_wig[,3])[2]-range(m_wig[,3])[1])))
                colnames(all_wig_hm)=c(tmpname,all_wig_name[ww])

              }

            }
          }
        }


        circos(R=360, cir=chrCircosDb, W=circosTrackWidth, mapping=all_wig_hm,  col.v=3,  type="heatmap2",  col.bar=TRUE, lwd=0.1, col="blue")
        circos(R=(360+circosTrackWidth), cir=chrCircosDb, W=1,   type="chr", print.chr.lab=FALSE, scale=TRUE)
        text(730,820,"track name Outside-to-inside",cex=0.55,family="mono")
        for(namei in 1:all_wig_num)
        {
          tmpwignum=regexpr(".wig",all_wig_name[namei])
          tmpwigname=substr(all_wig_name[namei],1,tmpwignum-1)
          text(740,820-namei*20,tmpwigname,cex=0.55,family="mono",col = "red")
        }
        dev.off() # text(400,820,family="mono",wigFile3,cex=0.7)
        print(paste(chrName," circos plot finish",sep = ""))


        rm(chrCmap)
        rm(chrBed)
        rm(chrBedBin)
        rm(chrBedMatrix)
        rm(chrCircosMapping)
        gc()
      }
    }
  }else
  {
    for (i in 1:chrNum)
    {

      tmpNum=regexpr(".matrix",matrix_name_dir[i])
      chrName=substr(matrix_name_dir[i],1,tmpNum-1)
      if(chrName==chrom)
      {
        chrBed=choose_chr_bed(m_bed,chrName)
        if(chrend>0)
        {
          chrBed=chrBed[which(chrBed[,3]<chrend),]
          chrBed[,2]=chrBed[,2]-chrstart
          chrBed[,3]=chrBed[,3]-chrstart
          chrBed=chrBed[which(chrBed[,2]>0),]
        }
        chrBedNum=dim(chrBed)[1]


        if(chrBedNum>0)
        {
          chrCmap=read.table(file=matrix_full_dir[i], fill=TRUE, stringsAsFactors=FALSE)

          chrTotSize=dim(chrCmap)[1]
          if(chrend>0)
          {
            tmpCmapStart=abs(ceiling((chrstart/(resolution*1000))))
            tmpCmapEnd=abs(ceiling((chrend/(resolution*1000))))
            if(tmpCmapEnd<tmpCmapStart)
            {
              print("please input correct start and end number")
              break
            }
            if(tmpCmapEnd>chrTotSize)
            {
              tmpCmapEnd=chrTotSize
            }
            if(tmpCmapStart>chrTotSize)
            {
              tmpCmapStart=chrTotSize
            }
            chrCmap=chrCmap[tmpCmapStart:tmpCmapEnd,tmpCmapStart:tmpCmapEnd]
            chrTotSize=dim(chrCmap)[1]
          }



          print(paste("plot ",chrName,"circos picture",sep=""))





          tmpNum2=regexpr("r",chrName)
          chrNo=substr(chrName,tmpNum2+1,nchar(chrName))
          #chrNo=as.numeric(chrNo)
          chrBedBin=check_bed_bin(chrBed,resolution*1000)
          chrBedMatrix=convert_bed_to_matrix(chrBed,resolution*1000,chrName,chrTotSize,bedWindow)



          chrCircosMapping=calculate_omiccircos_data(chrCmap,chrBedMatrix,chrBedBin,chrBed,chrName,chrCmap,if_threshold)
          if((dim(chrCircosMapping)[1])<5)
          {
            next;
          }
          chrCircosMapping[,1]=chrNo
          chrCircosMapping[,4]=chrNo
          chrCircosDb=segAnglePo(genomeFrame, seg=chrName)
          seg.num<-length(unique(genomeFrame[,1]))


          if((outputpdf==TRUE)||(outputpdf=="TRUE")||(outputpdf=="true"))
          {
            pdf(paste(matrix_dir,"/",chrName,"_circos.pdf",sep=""),width = 8,height = 8)

          }else
          {
            jpeg(paste(matrix_dir,"/",chrName,"_circos.jpeg",sep=""),width=1000,height=1000,quality = 100)
          }
          #pdf("tmp_circos.pdf",width = 8,height = 8)
          options(stringsAsFactors = FALSE);

          par(mar=c(2, 2, 2, 2));
          plot(c(1,800), c(1,800), type="n", axes=FALSE, xlab="", ylab="", main="");
          if(circosLinecolor=="rainbow")
          {
            colors<-rainbow(seg.num, alpha=0.5)
            circosLinecolor=colors
            circos(R=340, cir=chrCircosDb, W=40,  mapping=chrCircosMapping, type="link2", lwd=circosLineWidth,col=colors);
          }else if(circosLinecolor=="ReadCounts")
          {
            #colors<-c("#f7f4f9","#f7f4f9","#e7e1ef","#d4b9da","#c994c7","#df65b0","#e7298a","#ce1256","#980043","#67001f")
            colors<-c("#fff7f3","#fff7f3","#fde0dd","#fcc5c0","#fa9fb5","#f768a1","#dd3497","#b0017e","#7a0177","#49006a")
            #colors<-c("#fff7fb","#fff7fb","#ece7f2","#d0d1e6","#a6bddb","#74a9cf","#3690c0","#0570b0","#045a8d","#023858")
            #colors<-c("#ffffcc","#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#b00026")
            #colors<-c("#f7fbff","#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
            chrCircosMapping[,7]=as.numeric(chrCircosMapping[,7])
            chrCircosMapping<-chrCircosMapping[with(chrCircosMapping, order(read_count,decreasing = FALSE)), ]
            ccm_dim=dim(chrCircosMapping)[1]
            for(ccm in 1:10)
            {
              ccm_start=1+ceiling((ccm_dim/10)*(ccm-1))
              ccm_end=ceiling((ccm_dim/10)*ccm)
              circos(R=340, cir=chrCircosDb, W=40,  mapping=chrCircosMapping[ccm_start:ccm_end,], type="link2", lwd=circosLineWidth*ccm,col=colors[ccm]);
            }
          }else
          {
            circos(R=340, cir=chrCircosDb, W=40,  mapping=chrCircosMapping, type="link2", lwd=circosLineWidth,col=circosLinecolor);
          }

          all_wig_hm=NULL
          all_bed_wig_hm=NULL
          print(paste("plot ",chrName,"circos picture",sep=""))

          if(all_wig_num>0)
          {
            for(ww in 1:all_wig_num)
            {
              m_wig=load_wig(all_wig_file[ww],resolution*1000,chrName,chrTotSize,chrstart,chrend)


              chrBedWig=m_wig[which(chrBedMatrix[,1]==1),]
              chrBedWig[,2]=chrBedWig[,2]*resolution*1000
              m_wig[,2]=m_wig[,2]*resolution*1000

              m_wig_ttest=NULL
              if((length(chrBedWig))>0)
              {
                for(iii in 1:(dim(chrBedWig)[1]))
                {
                  m_wig_ttest=rbind(m_wig_ttest,t.test(m_wig[,3],mu=chrBedWig[iii,3]))
                }
                m_wig_equal=which(m_wig_ttest[,3]>0.05)
                m_wig_mean=mean(m_wig[,3])
                m_wig_nequal=which(m_wig_ttest[,3]<=0.05)
                m_wig_more=m_wig_nequal[which(chrBedWig[m_wig_nequal[],3]>m_wig_mean)]
                m_wig_less=m_wig_nequal[which(chrBedWig[m_wig_nequal[],3]<=m_wig_mean)]
                m_wig_bed=chrBedWig
                m_wig_bed[,3]=0
                m_wig_bed[m_wig_more,3]=1
                m_wig_bed[m_wig_less,3]=-1

                if(is.null(all_bed_wig_hm))
                {
                  all_bed_wig_hm=m_wig_bed
                }else
                {
                  all_bed_wig_hm=cbind(all_bed_wig_hm,m_wig_bed[,3])
                }
                if(is.null(all_wig_hm))
                {
                  all_wig_hm=m_wig
                  all_wig_hm[,3]=(all_wig_hm[,3]-range(all_wig_hm[,3])[1])/(range(all_wig_hm[,3])[2]-range(all_wig_hm[,3])[1])
                  tmpname=colnames(all_wig_hm)
                  colnames(all_wig_hm)=c(tmpname[1:2],all_wig_name[ww])
                }else
                {
                  tmpname=colnames(all_wig_hm)
                  all_wig_hm=cbind(all_wig_hm,((m_wig[,3]-range(m_wig[,3])[1])/(range(m_wig[,3])[2]-range(m_wig[,3])[1])))
                  colnames(all_wig_hm)=c(tmpname,all_wig_name[ww])

                }

              }
            }
          }


          circos(R=360, cir=chrCircosDb, W=circosTrackWidth, mapping=all_wig_hm,  col.v=3,  type="heatmap2",  col.bar=TRUE, lwd=0.1, col="blue")
          circos(R=(360+circosTrackWidth), cir=chrCircosDb, W=1,   type="chr", print.chr.lab=FALSE, scale=TRUE)
          text(730,820,"track name Outside-to-inside",cex=0.55,family="mono")
          for(namei in 1:all_wig_num)
          {
            tmpwignum=regexpr(".wig",all_wig_name[namei])
            tmpwigname=substr(all_wig_name[namei],1,tmpwignum-1)
            text(740,820-namei*20,tmpwigname,cex=0.55,family="mono",col = "red")
          }
          dev.off() # text(400,820,family="mono",wigFile3,cex=0.7)
          print(paste(chrName," circos plot finish",sep = ""))


          rm(chrCmap)
          rm(chrBed)
          rm(chrBedBin)
          rm(chrBedMatrix)
          rm(chrCircosMapping)
          gc()
        }
      }
    }
  }
}
