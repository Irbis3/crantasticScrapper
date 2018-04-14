#create an assembly
createAssembly <- function(name, size){
  return(data.frame(chr=name,start=0,end=size))
}

#get an assembly from a FASTA file
getAssemblyFromFasta <- function(fasta){
  chr <- character()
  sizes <- numeric()
  cont <- 0
  for(i in fasta){
    con <- file(i,'r')
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
      if(grepl("^>",oneLine)){
        cont <- cont+1
        chr[cont] <- sub(">","",unlist(strsplit(oneLine,"[ \\|]"))[1])
      }else if(is.na(sizes[cont])){
        sizes[cont] <- nchar(oneLine)
      }else{
        sizes[cont] <- sizes[cont]+nchar(oneLine)
      }
    }
    close(con)
  }
  return(data.frame(chr=chr,start=0,end=sizes))
}

# segmentation function
segmentation <- function(track, cell){
  track[,5] <- as.numeric(track[,5])
  track <- track[complete.cases(track[,5]),]

  cells <- ceiling(((track[,2]+track[,3])/2)/cell)
  aggdata <- aggregate(track[,5],by=list(track[,1],cells),FUN=mean)

  track <- data.frame(chr = aggdata[,1], start = (aggdata[,2]-1)*cell, end = aggdata[,2]*cell, name = NA, score = aggdata[,3], stringsAsFactors = FALSE)

  return(track)
}

#create json for genome map
genomemapJSON<-function(data, assembly){
  json <- list()
  json$chromosomes <- array(unique(assembly[,1]))
  json$data <- list()
  json$max <- max(assembly[,3])
  if(!(length(data)==1 && is.na(data))){
    if(is.character(data))
      data <- read.delim(data,FALSE,quote="")
    data[,5] <- as.numeric(data[,5])
    data <- data[complete.cases(data[,5]),]
    if(nrow(data)>100000){
      assembly_len <- sum(aggregate(assembly[,3],by=list(assembly[,1]),FUN=max)[,2])
      cell <- ceiling(assembly_len/100000)
      if(cell>1)
        data <- segmentation(data,cell)
    }
    for(i in json$chromosomes){
      subdata <- data.matrix(data[(data[,1]==i),c(2,3,5)])
      if(length(subdata)!=0)
        json$data[[as.vector(i)]] <- subdata
    }
    json$dataDomain <- c(min(data[,5]),max(data[,5]))
  }
  return(toJSON(json))
}

#create json for chromosomes' cytobands
chromosomesJSON<-function(assembly){
  json <- list()
  chr <- unique(assembly[,1])
  for(i in chr){
    table <- assembly[(assembly[,1]==i),-1]
    if(!is.data.frame(table))
      table <- data.frame(start = 0, end = table)
    json[[tolower(i)]] <- table
  }
  return(toJSON(json))
}

# open data base
openDB <- function(dir){
  db <- dbConnect(SQLite(), dbname = paste(dir, "Tracks.db", sep = "/"))
  res <- dbSendQuery(conn = db, "CREATE TABLE IF NOT EXISTS tbl_tracks (trackid INTEGER PRIMARY KEY, trackname TEXT, type TEXT, color TEXT, data TEXT)")
  dbClearResult(res)
  res <- dbSendQuery(conn = db, "CREATE TABLE IF NOT EXISTS tbl_segments (trackid INTEGER, chr TEXT COLLATE NOCASE, start INTEGER, end INTEGER, name TEXT, score TEXT, strand TEXT, thickStart INTEGER, thickEnd INTEGER, itemRGB TEXT, blockCount INTEGER, blockSizes TEXT, blockStarts TEXT)")
  dbClearResult(res)
  res <- dbSendQuery(conn = db, "CREATE INDEX IF NOT EXISTS location ON tbl_segments (chr,start,end)")
  dbClearResult(res)
  return(db)
}

# add tracks
add2DB <- function(db, track, trackname, type = "gene", color = "#000", trackdata = NULL){
  if(is.na(type))
    type = "gene"
  if(is.na(color))
    color = "#000"
  if(type %in% c("value","score")){
    track <- track[complete.cases(track[,5]),]
    if(!(length(trackdata)==2&&is.numeric(trackdata)))
      trackdata <- c(min(track[,5]),max(track[,5]))
    trackdata <- toJSON(trackdata)
  }else if(type %in% c("vcf","vcfsample")){
    if(is.null(trackdata))
      trackdata <- "{}"
    else
      trackdata <- toJSON(trackdata)
  }else{
    trackdata <- "[]"
  }
  sql <- paste0("INSERT INTO tbl_tracks VALUES (NULL,'",trackname,"','",type,"','",color,"','",gsub("'","''",trackdata,fixed=TRUE),"')")
  res <- dbSendQuery(conn = db, sql)
  dbClearResult(res)

  if(nrow(track)>2000000 && (type %in% c("value","score")))
      message("You can segmentate big amounts of data with segmentation function")

  if(length(track)>12){
    track <- track[,1:12]
    warning(paste(trackname,"have more than 12 columns"))
  }else{
    while(length(track)<12)
      track <- cbind(track,NA)
  }
  colnames(track) <- paste0('V',1:12)

  dbWriteTable(conn = db, name = "aux_segments", value = track)
  res <- dbSendQuery(conn = db, paste0("INSERT INTO tbl_segments SELECT (SELECT max(trackid) from tbl_tracks),",paste0(colnames(track),collapse=",")," FROM aux_segments"))
  dbClearResult(res)
  res <- dbSendQuery(conn = db, "DROP TABLE aux_segments")
  dbClearResult(res)
}

# close data base
closeDB <- function(db){
  res <- dbSendQuery(conn = db, "DROP TABLE IF EXISTS aux_genes")
  dbClearResult(res)
  res <- dbSendQuery(conn = db, "CREATE TABLE aux_genes AS SELECT chr, start, end, name, score FROM tbl_segments NATURAL JOIN tbl_tracks WHERE (type='gene' OR type='exons')")
  dbClearResult(res)
  dbDisconnect(db)
}

#create sqlite to store tracks
createDB<-function(dir, tracks = NA, types = NA, colors = NA){
  db <- openDB(dir)
  if(!(length(tracks)==1 && is.na(tracks))){
    for(i in seq_along(tracks)){
      add2DB(db, tracks[[i]], names(tracks)[i], types[i], colors[i])
    }
  }
  closeDB(db)
}

#track post-addition
genome_addTrack <- function(gb, track, trackname = NULL, type = "gene", color = "#000", scale = NA){
  if(is.data.frame(track)){
    if(is.null(trackname))
      trackname <- as.list(match.call())$track
  }else if(is.character(track) && file.exists(track)){
    if(is.null(trackname))
      trackname <- sub("^.*/","",track)
    track <- read.delim(track,FALSE,quote="")
  }else{
    warning("Track argument must be a data frame or a character vector giving an existing file.")
  }
  if(!is.null(trackname)){
    db <- openDB(gb$dir)
    add2DB(db, track, trackname, type, color, scale)
    closeDB(db)
  }
}

#add a sequence
genome_addSequence <- function(gb, fastafile){
  dir <- gb$dir
  if(!file.exists(paste(dir,"sequences",sep="/")))
    dir.create(paste(dir,"sequences",sep="/"))
  for(i in fastafile){
    con <- file(i,'r')
    seq <- ""
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
      if(grepl("^>",oneLine)){
        if(!is.character(seq))
          close(seq)
        chr <- unlist(strsplit(oneLine,"[ \\|]"))[1]
        seq <- file(paste0(dir,"/sequences/",sub(">","",chr),".fa"),"w")
        writeLines(chr,seq)
      }else{
        writeLines(oneLine,seq,sep="")
      }
    }
    close(seq)
    close(con)
  }
}

#create html wrapper for genome viewer
genomebrowser<-function(assembly, tracks = NA, types = NA, colors = NA, mapTrack = NA, server = FALSE, dir = "GenomeBrowser"){
  data <- genomemapJSON(mapTrack, assembly)
  chromosomes <- chromosomesJSON(assembly)
  if(server){
    createHTML(dir, c("d3.min.js","jspdf.min.js","functions.js","images.js","genomebrowser.js","genomemap.js"), data, chromosomes)
    file.copy(paste(wwwDirectory(), "query.php", sep = "/"), dir)
  }else{
    createHTML(dir, c("d3.min.js","jspdf.min.js","sql.js","functions.js","images.js","query.js","genomebrowser.js","genomemap.js"), data, chromosomes)
    message("Open the \"index.html\" file with Mozilla Firefox to see the graph. If you want to see this local file with other web browser, please visit the help section on the D3gb Web site http://d3gb.usal.es/")
  }
  createDB(dir, tracks, types, colors)
  structure(list(dir = dir, call = match.call()), class = "genomebrowser")
}

#create html wrapper for genome map
genomemap<-function(assembly, mapTrack = NA, dir = "GenomeMap"){
  createHTML(dir, c("d3.min.js","jspdf.min.js","functions.js","genomemap.js"), genomemapJSON(mapTrack, assembly), chromosomesJSON(assembly))
}

#add tracks to database
add_tracks <- function(dir,uniqTracks,tracks){
  db <- openDB(dir)
  for(currentTrack in uniqTracks){
    track <- tracks[as.vector(tracks[,1])==currentTrack,-1]
    color <- "black"
    type <- "domain"
    if(currentTrack %in% c("gene","CDS")){
      color <- "cadetblue"
      type <- "gene"
    }
    if(ncol(track) > 9 && !all(is.na(track[,10])) && max(track[,10],na.rm=TRUE) > 1){
      color <- "goldenrod"
      type <- "exons"
    }
    add2DB(db,track,currentTrack,type,color)
  }
  closeDB(db)
}

#create a genome viewer from a genBank file
gbk2genomebrowser <- function(gbkfile, server = FALSE, dir = "GenomeBrowser"){
  current <- 1
  currentTrack <- ""
  string <- ""
  sequence <- ""
  tmp <- tempdir()

  uniqTracks <- character()

  tracks <- character()
  scaffold <- character()
  start <- numeric()
  end <- numeric()
  name <- character()
  score <- character()
  strand <- character()

  blockCount <- numeric()
  blockSizes <- character()
  blockStarts <- character()

  chr <- character()
  chrLen <- numeric()

  trackData <- function(){
    if(string!=""){
      if(!grepl("^/",string)){
        tracks <<- c(tracks,currentTrack)
        scaffold <<- c(scaffold,chr[length(chr)])
        str <- "+"
        pos <- gsub("<|>","",string)
        if(grepl("complement",pos)){
          str <- "-"
          pos <- gsub("complement\\(|\\)","",pos)
        }
        if(grepl("join",pos)){
          pos <- gsub("join\\(|\\)","",pos)
          pos <- unlist(strsplit(pos,",",TRUE))
          blockCount[length(tracks)] <<- length(pos)
          pos <- lapply(strsplit(pos,"..",TRUE),as.numeric)
          pos <- pos[order(sapply(pos,function(x){ x[[1]] }))]
          start <<- c(start,pos[[1]][1]-1)
          end <<- c(end,pos[[length(pos)]][2])
          blockSizes[length(tracks)] <<- paste0(sapply(pos, function(x) x[2]-(x[1]-1)),collapse=",")
          blockStarts[length(tracks)] <<- paste0(sapply(pos, function(x) (x[1]-1)-start[length(start)]),collapse=",")
        }else{
          pos <- as.numeric(unlist(strsplit(pos,"..",TRUE)))
          start <<- c(start,pos[1]-1)
          end <<- c(end,pos[2])
        }
        strand <<- c(strand,str)
      }else if(grepl("/locus_tag=|/gene=",string)){
          name[length(tracks)] <<- gsub('.*="|"',"",string)
      }else if(!grepl("/translation=",string)){
          if(is.na(score[length(tracks)]))
            score[length(tracks)] <<- sub("^/","",string)
          else
            score[length(tracks)] <<- paste0(score[length(tracks)],sub("^/","|",string))
      }
      string <<- ""
    }
  }

  con <- file(gbkfile,'r')
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    aux <- unlist(strsplit(oneLine," +"))
    if(aux[1]=="LOCUS"){
      chr <- c(chr,aux[2])
      chrLen <- c(chrLen,aux[3])
      current <- 1
    }else if(aux[1]=="VERSION"){
      if(grepl(chr[length(chr)],aux[2]))
        chr[length(chr)] <- aux[2]
      current <- 1
    }else if(aux[1]=="FEATURES"){
      current <- 2
    }else if(aux[1]=="ORIGIN"){
      trackData()
      current <- 3
      sequence <- file(paste0(tmp,"/",chr[length(chr)],".fa"),"w")
      writeLines(paste0(">",chr[length(chr)]),con=sequence)
    }else if(grepl("^[A-Z]",aux[1])){
      current <- 1
    }else{
      if(current==2){
        if(substr(oneLine,6,6)!=" "){
          trackData()
          currentTrack <- aux[2]
          if(currentTrack!="source"){
            string <- paste0(string,aux[3])
            if(!(currentTrack %in% uniqTracks))
              uniqTracks <- c(uniqTracks,currentTrack)
          }
        }else{
          if(currentTrack!="source"){
            aux <- paste0(aux[-1],collapse=" ")
            if(grepl("^/",aux)){
              trackData()
              string <- paste0(string,aux)
            }else{
              string <- paste0(string,aux)
            }
          }
        }
      }
      if(current==3){
        if(aux[1]!="//")
          writeLines(gsub("[0-9]","",paste0(aux,collapse="")),con=sequence,sep="")
        else
          close(sequence)
      }
    }
  }
  close(con)
  assembly <- data.frame(chr,start=0,end=as.numeric(chrLen))
  length(name) <- length(tracks)
  length(score) <- length(tracks)
  tracks <- data.frame(tracks,scaffold,start,end,name,score,strand)
  if(length(blockCount)!=0){
    traLen <- length(scaffold)
    tracks[[8]] <- start
    tracks[[9]] <- end
    tracks[[10]] <- NA
    length(blockCount) <- traLen
    tracks[[11]] <- blockCount
    length(blockSizes) <- traLen
    tracks[[12]] <- blockSizes
    length(blockStarts) <- traLen
    tracks[[13]] <- blockStarts
  }
  gb <- genomebrowser(assembly,server=server,dir=dir)

  add_tracks(dir,uniqTracks,tracks)

  dir.create(paste(dir,"sequences",sep="/"))
  for(i in chr)
    file.copy(paste0(tmp,"/",i,".fa"),paste0(dir,"/sequences/",i,".fa"))

  unlink(tmp)

  return(gb)
}

#add a gff file to genome viewer
genome_addGFF <- function(gb, gfffile){

  gff <- read.delim(gfffile,FALSE,quote="",comment.char="#")
  nrows <- nrow(gff)

  ID <- as.character(seq_len(nrows))
  trackName <- gff[,3]
  scaffold <- gff[,1]
  start <- as.numeric(gff[,4])-1
  end <- as.numeric(gff[,5])
  name <- character(nrows)
  score <- character(nrows)
  strand <- sub("[^+-]",NA,gff[,7])
  thickStart <- numeric(nrows)
  thickEnd <- numeric(nrows)

  uniqTracks <- unique(trackName)
  uniqTracks <- uniqTracks[which(uniqTracks!="exon")]

  for(cont in seq_len(nrows)) {
      attrs <- unlist(strsplit(as.character(gff[cont,9]),";"))
      scr <- NA
      idx <- grep("^ID=",attrs)
      id <- sub("^ID=","",attrs[idx])
      if(trackName[cont] == "exon"){
        nax <- grep("^Parent=",attrs)
        nam <- sub("^Parent=","",attrs[nax])
      }else{
        if(length(id)!=0)
          ID[cont] <- id
        nax <- grep("^Name=",attrs)
        nam <- sub("^Name=","",attrs[nax]) 
        if(length(nax)!=0 || length(idx)!=0)
          attrs <- attrs[-c(nax,idx)]
        scr <- paste0(attrs,collapse="|")
      }
      if(length(nam)==0){
        nam <- NA
        if(length(id)!=0)
          nam <- id
      }
      name[cont] <- nam
      score[cont] <- scr
      if(gff[cont,8] %in% c("1","2")){
        if(strand[cont]!="-"){
          thickStart[cont] <- start[cont]+as.numeric(gff[cont,8])
          thickEnd[cont] <- end[cont]
        }else{
          thickStart[cont] <- start[cont]
          thickEnd[cont] <- end[cont]-as.numeric(gff[cont,8])
        }
      }
  }

  tracks <- data.frame(trackName, scaffold, start, end, name, score, strand, thickStart, thickEnd)

  if(length(ID)>length(unique(ID))){
    warning("duplicate 'IDs' are not allowed")
    tracks <- tracks[!duplicated(ID),]
    ID <- ID[!duplicated(ID)]
  }

  rownames(tracks) <- ID

  if("exon" %in% trackName){
    exons <- tracks[tracks[,1]=="exon",]
    tracks <- tracks[tracks[,1]!="exon",]
    uniqueParent <- unique(exons[,5])
    uniqueParent <- uniqueParent[complete.cases(uniqueParent)]
    tracks[["itemRGB"]] <- NA
    tracks[["blockCount"]] <- NA
    tracks[["blockSizes"]] <- NA
    tracks[["blockStarts"]] <- NA

    for(i in uniqueParent){
      index <- which(i==exons[,5])
      tracks[i,"blockCount"] <- length(index)
      tracks[i,"blockSizes"] <- paste0((exons[index,4]-(exons[index,3])),collapse=",")
      tracks[i,"blockStarts"] <- paste0(((exons[index,3])-tracks[i,3]),collapse=",")
    }
  }

  add_tracks(gb$dir,uniqTracks,tracks)
}

#add vcf track
genome_addVCF <- function(gb, vcffile, trackname=NULL, show=NULL){

  get_description <- function(line){
    id <- gsub(".+ID=|,.+$","",line)[1]
    desc <- gsub(".+Description=\"|\".+$","",line)[1]
    return(c(id,desc))
  }

  if(!is.character(trackname))
    trackname <- sub("^.*/","",vcffile)

  uniqTracks <- character()

  name <- character()
  chr <- character()
  start <- numeric()
  ID <- character()
  info <- character()

  datainfo <- list()
  dataformat <- list()
  datacsq <- character()

  cont <- 0

  con <- file(vcffile,'r')
  ntracks <- length(grep('^[^#]',readLines(con)))
  seek(con,1)
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    if(grepl("^##INFO=<ID=",oneLine)){
      desc <- get_description(oneLine)
      if(desc[1] == 'CSQ')
        datacsq <- unlist(strsplit(sub("^.+Format: ","",desc[2]),"|",TRUE))
      else
        datainfo[[desc[1]]] <- desc[2]
    }else if(grepl("^##FORMAT=<ID=",oneLine)){
      desc <- get_description(oneLine)
      dataformat[[desc[1]]] <- desc[2]
    }else if(grepl("^#CHROM",oneLine)){
      aux <- unlist(strsplit(oneLine,"\t"))
      uniqTracks <- aux[-(1:9)]
      ntracks <- ntracks*(length(uniqTracks)+1)
      name <- character(ntracks)
      chr <- character(ntracks)
      start <- numeric(ntracks)
      ID <- character(ntracks)
      info <- character(ntracks)
    }else if(!grepl("^#",oneLine)){
      aux <- unlist(strsplit(oneLine,"\t"))
      cont <- cont+1
      name[cont] <- trackname
      chr[cont] <- aux[1]
      start[cont] <- aux[2]
      ID[cont] <- aux[3]

      score <- paste0("REF=",aux[4])
      if(aux[5]!='.')
        score <- c(score,paste0("ALT=",aux[5]))
      if(aux[6]!='.')
        score <- c(score,paste0("QUAL=",aux[6]))
      if(!is.na(aux[8])){
        inf <- unlist(strsplit(aux[8],";"))
        for(infi in inf){
          if(grepl("^CSQ=",infi)){
            csq <- unlist(strsplit(sub("^CSQ=","",infi),","))[1]
            csq <- unlist(strsplit(csq,"|",TRUE))
            for(i in seq_along(datacsq))
              if(!is.na(csq[i]) && csq[i]!="")
                score <- c(score,paste0(datacsq[i],"=",csq[i]))
          }else
            score <- c(score,infi)
        }
      }
      info[cont] <- paste0(score,collapse="|")

      if(!is.na(aux[9])){
        format <- unlist(strsplit(aux[9],":"))
        for(i in seq_along(uniqTracks)){
          attr <- unlist(strsplit(aux[9+i],":"))
          if(length(format)==length(attr)){
            cont <- cont+1
            name[cont] <- uniqTracks[i]
            chr[cont] <- aux[1]
            start[cont] <- aux[2]
            ID[cont] <- aux[3]
            for(j in seq_along(format))
              attr[j] <- paste(format[j],attr[j],sep="=")
            info[cont] <- paste0(attr,collapse="|")
          }
        }
      }
    }
  }
  close(con)

  ID[ID=="."] <- NA
  tracks <- data.frame(name,chr,as.numeric(start)-1,start,ID,info)

  db <- openDB(gb$dir)
  datavcf <- NULL
  if(length(datainfo)){
    datavcf <- list(info=datainfo)
    if(!is.null(show))
      datavcf[["show"]] <- show
  }
  if(!length(dataformat))
    dataformat <- NULL
  add2DB(db,tracks[as.vector(tracks[,1])==trackname,-1],trackname,'vcf',"#000",datavcf)
  for(i in seq_along(uniqTracks))
    add2DB(db,tracks[as.vector(tracks[,1])==uniqTracks[i],-1],uniqTracks[i],'vcfsample',"#000",dataformat)
  closeDB(db)
}
