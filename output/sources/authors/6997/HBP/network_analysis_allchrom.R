network_analysis_allchrom<-function(bedFile,matrix_dir="hg19",outputpdf=FALSE,chrom="all",chrstart=0,chrend=0,resolution=100,bedWindow=0,net_layout="layout.fruchterman.reingold",netplot=TRUE,NetClusterType="multileve",NetVertexSize=2,NetVertexChangeSize="degree",NetVertexLableDist=0.1,NetVertexColor="#7fbc41",NetVertexLabelCex=3,if_threshold=0)
{
  matrix_name_dir=list.files(path=matrix_dir,full.names=F,pattern=".matrix")
  matrix_full_dir=list.files(path=matrix_dir,full.names=T,pattern=".matrix")
  m_bed=load_bed(bedFile)
  memcolor<-c("#8dd3c7","#ffffbc","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd")
  weight="weight"
  deg="deg"
  Var1="Var1"
  Var2="Var2"
  value="value"
  bed="bed"
  chrNum=length(matrix_name_dir)
  for (i in 1:chrNum)
  {
    tmpNum=regexpr(".matrix",matrix_name_dir[i])
    chrName=substr(matrix_name_dir[i],1,tmpNum-1)
    chrBed=choose_chr_bed(m_bed,chrName)
    chrBedNum=dim(chrBed)[1]
    print(matrix_full_dir[i])
    if(chrBedNum>0)
    {
      print(chrName)
      chrCmap=read.table(file=matrix_full_dir[i], fill=TRUE, stringsAsFactors=FALSE)
      chrTotSize=dim(chrCmap)[1]
      chrBedBin=check_bed_bin(chrBed,resolution*1000)
      chrBedMatrix=convert_bed_to_matrix(chrBed,resolution*1000,chrName,chrTotSize,bedWindow)
      chrBedToBedInter=find_bed_to_bed_interaction(chrCmap,chrBedMatrix,chrBedBin,chrBed,chrName,chrCmap,if_threshold)
      if((dim(chrBedToBedInter)[1])<5)
      {
        next;
      }
      if(((netplot==TRUE)||(netplot=="true")||(netplot=="TRUE")))
      {
        netgraph=data.frame("p1"=character(dim(chrBedToBedInter)[1]),"p2"=character(dim(chrBedToBedInter)[1]),"weight"=numeric(dim(chrBedToBedInter)[1]))
        netgraph[,1]=as.data.frame(paste(chrBedToBedInter[,2],":",(chrBedToBedInter[,3]+chrstart),"-",(chrBedToBedInter[,4]+chrstart),sep=""))
        netgraph[,2]=as.data.frame(paste(chrBedToBedInter[,8],":",(chrBedToBedInter[,9]+chrstart),"-",(chrBedToBedInter[,10]+chrstart),sep=""))
        netgraph[,3]=as.data.frame(chrBedToBedInter[,13])
        set.seed(1234)
        if((outputpdf==TRUE)||(outputpdf=="TRUE")||(outputpdf=="true"))
        {
          pdf(paste(matrix_dir,"/",chrName,"_netplot.pdf",sep=""),width = 8,height = 8)

        }else
        {
          jpeg(paste(matrix_dir,"/",chrName,"_netplot.jpeg",sep=""),width=1000,height=1000,quality = 100)
        }
        g = graph.data.frame(netgraph,directed = F)
        set.seed(1234)
        netnodename=names(V(g))
        netnodechrnum=regexpr(":",netnodename)
        netnodechr=substr(netnodename,1,netnodechrnum-1)
        netnodestartnum=regexpr("-",netnodename)
        netnodestart=as.numeric(substr(netnodename,netnodechrnum+1,netnodestartnum-1))
        netnodeendnum=nchar(netnodename)
        netnodeend=as.numeric(substr(netnodename,netnodestartnum+1,netnodeendnum))
        netcsv=data.frame("chrom"=character(0),"start"=numeric(0),"end"=numeric(0),"degree"=numeric(0),"closeness"=numeric(0),"betweenness"=numeric(0),"Local_cluster_coefficient"=numeric(0),"Eigenvector_centrality"=numeric(0),"membership"=numeric(0),stringsAsFactors=FALSE)
        netdegree=degree(g)
        netcloseness=closeness(g)
        netbetweenness=betweenness(g)
        netcoefficient=transitivity(g, type="local")
        netcentrality=evcent(g)$vector

        netcsv[1:(length(netdegree)),1]=netnodechr
        netcsv[1:(length(netdegree)),2]=netnodestart
        netcsv[1:(length(netdegree)),3]=netnodeend

        netcsv[1:(length(netdegree)),4]=as.data.frame(netdegree)
        netcsv[1:(length(netdegree)),5]=as.data.frame(netcloseness)
        netcsv[1:(length(netdegree)),6]=as.data.frame(netbetweenness)
        netcsv[1:(length(netdegree)),7]=as.data.frame(netcoefficient)
        netcsv[1:(length(netdegree)),8]=as.data.frame(netcentrality)
        colors<-c("#fff7f3","#fff7f3","#fde0dd","#fcc5c0","#fa9fb5","#f768a1","#dd3497","#b0017e","#7a0177","#49006a")
        weight_range=range(E(g)$weight)
        E(g)$color=colors[1]
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])/10)]$color=colors[2]
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*2/10)]$color=colors[3]
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*3/10)]$color=colors[4]
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*4/10)]$color=colors[5]
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*5/10)]$color=colors[6]
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*6/10)]$color=colors[7]
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*7/10)]$color=colors[8]
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*8/10)]$color=colors[9]
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*9/10)]$color=colors[10]

        edge_width=0.05
        E(g)$width=edge_width
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])/10)]$width=2*edge_width
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*2/10)]$width=3*edge_width
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*3/10)]$width=4*edge_width
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*4/10)]$width=5*edge_width
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*5/10)]$width=6*edge_width
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*6/10)]$width=7*edge_width
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*7/10)]$width=8*edge_width
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*8/10)]$width=9*edge_width
        E(g)[weight>=(weight_range[1]+(weight_range[2]-weight_range[1])*9/10)]$width=10*edge_width

        if(NetVertexChangeSize=="degree")
        {
          V(g)$deg<-netcsv[,4]
          deg_range=range(netcsv[,4])
          V(g)$size=NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])/5)]$size=2*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*2/5)]$size=3*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*3/5)]$size=4*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*4/5)]$size=5*NetVertexSize
        }else if(NetVertexChangeSize=="closeness")
        {
          V(g)$deg<-netcsv[,5]
          deg_range=range(netcsv[,5])
          V(g)$size=NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])/5)]$size=2*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*2/5)]$size=3*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*3/5)]$size=4*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*4/5)]$size=5*NetVertexSize
        }else if(NetVertexChangeSize=="betweenness")
        {
          V(g)$deg<-netcsv[,6]
          deg_range=range(netcsv[,6])
          V(g)$size=NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])/5)]$size=2*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*2/5)]$size=3*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*3/5)]$size=4*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*4/5)]$size=5*NetVertexSize
        }else if(NetVertexChangeSize=="Local_cluster_coefficient")
        {
          V(g)$deg<-netcsv[,7]
          deg_range=range(netcsv[,7])
          V(g)$size=NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])/5)]$size=2*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*2/5)]$size=3*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*3/5)]$size=4*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*4/5)]$size=5*NetVertexSize
        }else if(NetVertexChangeSize=="Eigenvector_centrality")
        {
          V(g)$deg<-netcsv[,8]
          deg_range=range(netcsv[,8])
          V(g)$size=NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])/5)]$size=2*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*2/5)]$size=3*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*3/5)]$size=4*NetVertexSize
          V(g)[deg>=(deg_range[1]+(deg_range[2]-deg_range[1])*4/5)]$size=5*NetVertexSize
        }else
        {
          V(g)$size=NetVertexSize
        }

        memcolor<-c("#8dd3c7","#ffffbc","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd")

        if(net_layout=="layout.fruchterman.reingold")
        {

          if(NetClusterType=="NULL")
          {
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, vertex.color=NetVertexColor, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            netcsv[1:(length(netdegree)),9]="NULL"
          }
          if(NetClusterType=="edgeBetweenness")
          {
            system.time(ec <- edge.betweenness.community(g))

            print(modularity(ec))
            netcsv[1:(length(netdegree)),9]=ec$membership
            net_mem=ec$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }

            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
          }
          if(NetClusterType=="walktrap")
          {
            system.time(wc <- walktrap.community(g))
            netcsv[1:(length(netdegree)),9]=wc$membership

            print(modularity(wc))
            net_mem=wc$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)

          }
          if(NetClusterType=="multileve")
          {
            system.time(mc <- multilevel.community(g, weights=NA))
            print(modularity(mc))
            net_mem=mc$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            netcsv[1:(length(netdegree)),9]=mc$membership

          }
          if(NetClusterType=="labelPropagation")
          {
            system.time(lc <- label.propagation.community(g))
            print(modularity(lc))
            net_mem=lc$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            netcsv[1:(length(netdegree)),9]=lc$membership

          }
        }else if(net_layout=="layout.circle")
        {
          if(NetClusterType=="NULL")
          {
            plot(g,layout=layout.circle, vertex.label.dist=NetVertexLableDist, vertex.color=NetVertexColor, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            netcsv[1:(length(netdegree)),9]="NULL"
          }
          if(NetClusterType=="edgeBetweenness")
          {
            system.time(ec <- edge.betweenness.community(g))
            print(modularity(ec))
            netcsv[1:(length(netdegree)),9]=ec$membership
            net_mem=ec$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
          }
          if(NetClusterType=="walktrap")
          {
            system.time(wc <- walktrap.community(g))
            netcsv[1:(length(netdegree)),9]=wc$membership

            print(modularity(wc))
            net_mem=wc$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            #plot(wc , g,layout=layout.circle,vertex.label.dist=NetVertexLableDist, vertex.color=NetVertexColor, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)

          }
          if(NetClusterType=="multileve")
          {
            system.time(mc <- multilevel.community(g, weights=NA))
            print(modularity(mc))
            net_mem=mc$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            #plot(mc, g,layout=layout.circle,vertex.label.dist=NetVertexLableDist, vertex.color=NetVertexColor, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            netcsv[1:(length(netdegree)),9]=mc$membership

          }
          if(NetClusterType=="labelPropagation")
          {
            system.time(lc <- label.propagation.community(g))
            print(modularity(lc))
            net_mem=lc$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            netcsv[1:(length(netdegree)),9]=lc$membership

          }
        }else
        {
          if(NetClusterType=="NULL")
          {
            plot(g,layout=layout.auto, vertex.label.dist=NetVertexLableDist, vertex.color=NetVertexColor, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            netcsv[1:(length(netdegree)),9]="NULL"
          }
          if(NetClusterType=="edgeBetweenness")
          {
            system.time(ec <- edge.betweenness.community(g))
            print(modularity(ec))
            netcsv[1:(length(netdegree)),9]=ec$membership
            net_mem=ec$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
          }
          if(NetClusterType=="walktrap")
          {
            system.time(wc <- walktrap.community(g))
            netcsv[1:(length(netdegree)),9]=wc$membership

            print(modularity(wc))
            net_mem=wc$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)

          }
          if(NetClusterType=="multileve")
          {
            system.time(mc <- multilevel.community(g, weights=NA))
            print(modularity(mc))
            net_mem=mc$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            #plot(mc, g,layout=layout.auto,vertex.label.dist=NetVertexLableDist, vertex.color=NetVertexColor, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            netcsv[1:(length(netdegree)),9]=mc$membership

          }
          if(NetClusterType=="labelPropagation")
          {
            system.time(lc <- label.propagation.community(g))
            print(modularity(lc))
            net_mem=lc$membership
            mem_max=max(net_mem)
            if(mem_max>10)
            {
              memcolor=rainbow(mem_max, alpha=0.5)
            }
            for(memi in 1:mem_max)
            {
              V(g)[net_mem==memi]$color=memcolor[memi]

            }
            plot(g,layout=layout.fruchterman.reingold, vertex.label.dist=NetVertexLableDist, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            #plot(lc , g,layout=layout.auto,vertex.label.dist=NetVertexLableDist, vertex.color=NetVertexColor, edge.arrow.size=0.05,vertex.label.cex=NetVertexLabelCex)
            netcsv[1:(length(netdegree)),9]=lc$membership

          }
        }

        dev.off()

        write.csv(netcsv,paste(matrix_dir,"/",chrName,"_network.csv",sep=""),row.names = FALSE)
      }

      if((dim(chrBedToBedInter)[1])<5)
      {
        next;
      }
      bedIplot=cbind(rbind(chrBedToBedInter[,14],chrBedToBedInter[,15]),rbind(chrBedToBedInter[,15],chrBedToBedInter[,14]))
      write.table(chrBedToBedInter,file=paste(matrix_dir,"/",chrName,"_BedToBedInter.txt",sep=""),sep="\t",row.names=FALSE,col.names=FALSE,quote=FALSE)

      hm_dim=dim(chrCmap)[1]
      chrCmap=as.matrix(chrCmap)
      hm_mean=mean(chrCmap)


      bedhmmatrix=matrix(data = 0,nrow = hm_dim,ncol = hm_dim)
      for (bedi in 1:dim(bedIplot)[2])
      {

        bedhmmatrix[bedIplot[1,bedi],bedIplot[2,bedi]]=chrCmap[bedIplot[1,bedi],bedIplot[2,bedi]]

      }
      rownames(bedhmmatrix)=rownames(chrCmap)
      colnames(bedhmmatrix)=rownames(chrCmap)

      bedhmcmap=melt(bedhmmatrix)

      for(j in 1:hm_dim)
      {

        chrCmap[j,which(chrCmap[j,]>5*hm_mean)]=5*hm_mean
      }
      colnames(chrCmap)=rownames(chrCmap)

      chrhmCmap=melt(chrCmap)
      print(paste("plot ",chrName,"bed picture",sep=""))
      if((outputpdf==TRUE)||(outputpdf=="TRUE")||(outputpdf=="true"))
      {
        pdf(paste(matrix_dir,"/",chrName,"_bedplot.pdf",sep=""),width = 8,height = 8)

      }else
      {
        jpeg(paste(matrix_dir,"/",chrName,"_bedplot.jpeg",sep=""),width=1000,height=1000,quality = 100)
      }

      grid.newpage()
      heatmapViewport <- viewport(height=0.5, width=0.5, x=0.25,y=0.5)
      scatterViewport <- viewport(height=0.5, width=0.5, x=0.75,y=0.5)
      densityViewport <- viewport(height=0.25,width=0.5, x=0.75,y=0.125)
      hmdensityViewport <- viewport(height=0.25,width=0.5,x=0.25,y=0.125)
      jit=position_jitter(width=0.5)
      hmrange=range(chrhmCmap[,1])
      #print(bedIplot)
      #print(hmrange)
      bedIplot=bedIplot+hmrange[1]
      chrhm = ggplot(chrhmCmap, aes(x=Var1, y=Var2, fill=value))+scale_y_discrete(breaks=seq(0, 10, 5))+xlab('chrom')+ylab("chrom")+scale_fill_gradient(low='white', high='red')+geom_tile()+guides(fill=FALSE)
      #chrbedplot=qplot(bedIplot[1,],bedIplot[2,],alpha=I(1/10),size=I(1))+xlab('chrom')+ylab("chrom")+geom_jitter(position=jit,colour="black",alpha=1/100)
      chrbedplot = ggplot(bedhmcmap, aes(x=Var1, y=Var2, fill=value))+scale_y_discrete(breaks=seq(0, 10, 5))+xlab('chrom')+ylab("chrom")+scale_fill_gradient(low='white', high='black')+geom_tile()+guides(fill=FALSE)

      chrbeddensitydata=as.data.frame(c(chrBedToBedInter[,14],chrBedToBedInter[,15]))
      chrbeddensitydata=chrbeddensitydata+hmrange[1]
      colnames(chrbeddensitydata)="bed"
      chrbeddensity=ggplot(chrbeddensitydata)+geom_density(aes(x=bed))

      chrhmdensitydata=NULL
      for(iiii in 1:chrTotSize)
      {
        #pp[iiii,1]=length(which(chrCmap[,iiii]>0))
        chrhmdensitydata=c(chrhmdensitydata,which(chrCmap[,iiii]>if_threshold))
      }
      chrhmdensitydata=as.data.frame(chrhmdensitydata)
      chrhmdensitydata=chrhmdensitydata+hmrange[1]
      colnames(chrhmdensitydata)="chrom"


      chrcmapdensity=ggplot(chrhmdensitydata)+geom_density(aes(x=chrom))



      print(chrhm,vp=heatmapViewport)
      print(chrbedplot,vp=scatterViewport)
      print(chrbeddensity,vp=densityViewport)
      print(chrcmapdensity,vp=hmdensityViewport)

      dev.off()

      write.table(chrBedToBedInter,file=paste(matrix_dir,"/",chrName,"_BedToBedInter.txt",sep=""),sep="\t",row.names=FALSE,col.names=FALSE,quote=FALSE)
      rm(chrCmap)
      rm(chrBed)
      rm(chrBedBin)
      rm(chrBedMatrix)
      rm(chrBedToBedInter)
      gc()
    }
  }
}
