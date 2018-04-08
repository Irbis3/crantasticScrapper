generate_enzyme_file<-function(enzyme="HindIII",enzymesite="AAGCTT",chrom_file="chrom_hg19.sizes",enzymedir="annotation",enzymeoverhangs5=1,genomeName="hg19")
{
  chrom_info=read.table(chrom_file,fill=TRUE, stringsAsFactors=FALSE)
  extdata_file=system.file("extdata", package="HBP")
  all_genomedb=read.csv(paste(extdata_file,"/all_genome_db.csv",sep = ""),header=FALSE)
  enzymesitesfile=paste(enzyme,"_resfrag_",genomeName,".bed",sep="")
  enzyme_dir=list.files(path=enzymedir,full.names=F,pattern=enzymesitesfile)
  if(length(enzyme_dir)==0)
  {
    for(i in 1:length(all_genomedb))
    {
      dbfind=-1
      dbrequire=FALSE
      r_genomedb=NULL
      dbfind=regexpr(genomeName,all_genomedb[1,i])
      if(dbfind!=-1)
      {
        r_genomedb=as.character(all_genomedb[1,i])
        dbrequire=require(all_genomedb[1,i],character.only = TRUE)
        break
      }
    }
    if(!is.null(r_genomedb))
    {
      if(dbrequire==FALSE)
      {
        print(paste("try to install the R package ",r_genomedb,sep=""))
        source("http://www.bioconductor.org/biocLite.R")
        biocLite(r_genomedb)
      }else
      {
        all_chr <- chrom_info[,1]
        resFrag <- getRestrictionFragmentsPerChromosome(resSite=enzymesite, chromosomes=all_chr, overhangs5=enzymeoverhangs5, genomePack=r_genomedb)
        allRF <- do.call("c",resFrag)
        names(allRF) <- unlist(sapply(resFrag, function(x){paste0("HIC_", seqlevels(x), "_", 1:length(x))}))
        if(file.exists(enzymedir)==FALSE)
        {
          dir.create(enzymedir)
        }
        export(allRF, format="bed", con=paste(enzymedir,"/",enzymesitesfile,sep=""))
      }
    }else
    {
      print(paste("can not find the genome file",sep=""))
    }
  }else
  {
    print(paste("enzymesites file ",enzymesitesfile," is already existed, skip to generate it"))
  }
}

run_hicpro<-function(hicpro_path="HiC-Pro",inputfile="rawdata",configfile="config-hicpro",outdir="hg19")
{
  hiccmd=paste(hicpro_path," -i ",inputfile," -o ",outdir," -c ",configfile,sep="")
  print(hiccmd)
  system(hiccmd)
}
