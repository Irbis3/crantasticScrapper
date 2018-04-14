#   Data preparation with R Bioconductor TCGAbiolinks package.  We need  
#   paired tumor/normal samples inlcuded in all TCGA LIHC datasets.
#
#   Reference: 
#   https://www.bioconductor.org/packages/release/bioc/html/TCGAbiolinks.html
#
#   Last updated in May 10, 2016 by Henry Zhang (hzhang@mail.nih.gov)
#   ______________________________________________________________________
#   <TCGAbiolinks><TCGAbiolinks><TCGAbiolinks><TCGAbiolinks><TCGAbiolinks>



library("TCGAbiolinks")
dir.create("TCGAbiolinks.Download")
filePath <- "TCGAbiolinks.Download/"


#   1.  Select paired tumor/normal samples included in all data sets
#
#   ======================================================================


    #   Clinical information table. 
    #   
    lihc.clinical <- TCGAquery_clinic("LIHC","biospecimen_sample");

    dim(lihc.clinical)    #   [1] 796  28
    colnames(lihc.clinical)

    # [1] "disease"                            "bcr_patient_uuid"                  
    # [3] "bcr_sample_barcode"                 "bcr_sample_uuid"                   
    # [5] "biospecimen_sequence"               "composition"                       
    # [7] "current_weight"                     "days_to_collection"                
    # [9] "days_to_sample_procurement"         "freezing_method"                   
    #[11] "initial_weight"                     "intermediate_dimension"            
    #[13] "is_ffpe"                            "longest_dimension"                 
    #[15] "method_of_sample_procurement"       "oct_embedded"                      
    #[17] "other_method_of_sample_procurement" "pathology_report_file_name"        
    #[19] "pathology_report_uuid"              "preservation_method"               
    #[21] "sample_type"                        "sample_type_id"                    
    #[23] "shortest_dimension"          "time_between_clamping_and_freezing"
    #[25] "time_between_excision_and_freezing" "tissue_type"                       
    #[27] "tumor_descriptor"                    "vial_number" 

    save(lihc.clinical, file=paste0(filePath,"TCGA.LIHC.Clinical.Data.RData"))
    unlink("nationwidechildrens.org_LIHC.bio.Level_2.0.56.0", 
        recursive=TRUE, force=TRUE)


    #   1.1     Paired sample only
    #   
    solid.tissue.rows <- which(lihc.clinical$sample_type_id %in% c(1, 11))
    lihc.samples <- data.frame(
        substr(lihc.clinical$bcr_sample_barcode[solid.tissue.rows], 1, 12),
        lihc.clinical$bcr_sample_barcode[solid.tissue.rows],
        lihc.clinical$sample_type[solid.tissue.rows],
        lihc.clinical$sample_type_id[solid.tissue.rows]);
    colnames(lihc.samples) <- c("sample_ID", "barcode", "tissue_type", 
                                    "tissue_ID")

    sampleID <- lihc.samples$sample_ID
    sampleID <- sampleID[which(duplicated(sampleID))]
    lihc.samples <- lihc.samples[which(lihc.samples$sample_ID %in% sampleID), ]

    dim(lihc.samples)           #   [1] 178   4
    head(lihc.samples)

    #      sample_ID          barcode         tissue_type tissue_ID
    # 2 TCGA-BC-A10Q TCGA-BC-A10Q-01A       Primary Tumor         1
    # 3 TCGA-BC-A10Q TCGA-BC-A10Q-11A Solid Tissue Normal        11
    # 4 TCGA-BC-A10R TCGA-BC-A10R-01A       Primary Tumor         1
    # 5 TCGA-BC-A10R TCGA-BC-A10R-11A Solid Tissue Normal        11
    # 6 TCGA-BC-A10S TCGA-BC-A10S-01A       Primary Tumor         1
    # 7 TCGA-BC-A10S TCGA-BC-A10S-11A Solid Tissue Normal        11


    #   Data queries for sample selection. Sample barcodes for each 
    #   dataset are included in each query
    #   
    lihc.RNAseq.query   <- TCGAquery(tumor="LIHC", level="3", 
                    platform="IlluminaHiSeq_RNASeqV2")
    lihc.miRNAseq.query <- TCGAquery(tumor="LIHC", level="3", 
                    platform="IlluminaHiSeq_miRNASeq")
    lihc.methyl.query   <- TCGAquery(tumor="LIHC", level="3", 
                    platform="HumanMethylation450")
    lihc.SNP.query <- TCGAquery(tumor="LIHC", level="3", 
                    platform="Genome_Wide_SNP_6")
    lihc.exome.query <- TCGAquery(tumor="LIHC", level="2", 
                    platform="IlluminaGA_DNASeq_automated")


    #   1.2     Select the common samples in all datasets by barcode.  
    #           Each time, the output may contains unpaired sample(s)
    #   

    lihc.barcode <- as.character(lihc.samples$barcode)
    length(lihc.barcode)    #   [1] 178

    #   filter by RNAseq samples
    #  
    RNAseq.barcode <- strsplit(lihc.RNAseq.query$barcode, split=",")[[1]]
    RNAseq.barcode <- substr(RNAseq.barcode, 1, 16)
    lihc.barcode <- lihc.barcode[which(lihc.barcode %in% RNAseq.barcode)]

    sampleID <- substr(lihc.barcode, 1, 12)
    pairedID <- sampleID[which(duplicated(sampleID))]
    lihc.barcode <- lihc.barcode[which(sampleID %in% pairedID)]
    length(pairedID)        #   [1] 50
    length(lihc.barcode)    #   [1] [1] 100

    #   filter by miRNA samples
    #   
    miRNAseq.barcode <- strsplit(lihc.miRNAseq.query$barcode, split=",")[[1]]
    miRNAseq.barcode <- substr(miRNAseq.barcode, 1, 16)
    lihc.barcode <- lihc.barcode[which(lihc.barcode %in% miRNAseq.barcode)]

    sampleID <- substr(lihc.barcode, 1, 12)
    pairedID <- sampleID[which(duplicated(sampleID))]
    lihc.barcode <- lihc.barcode[which(sampleID %in% pairedID)]
    length(pairedID)        #   [1] 49
    length(lihc.barcode)    #   [1] 98

    #   filter by methylation samples
    #   
    methyl.barcode <- paste(lihc.methyl.query$barcode, collapse=",")
    methyl.barcode <- strsplit(methyl.barcode, split=",")[[1]]
    methyl.barcode <- substr(methyl.barcode, 1, 16)
    lihc.barcode <- lihc.barcode[which(lihc.barcode %in% methyl.barcode)]

    sampleID <- substr(lihc.barcode, 1, 12)
    pairedID <- sampleID[which(duplicated(sampleID))]
    lihc.barcode <- lihc.barcode[which(sampleID %in% pairedID)]

    length(pairedID)        #   [1] 41
    length(lihc.barcode)    #   [1] 82

    #   filter by Genome_Wide_SNP_6 samples
    #   
    SNP.barcode <- paste(lihc.SNP.query$barcode, collapse=",")
    SNP.barcode <- strsplit(SNP.barcode, split=",")[[1]]
    SNP.barcode <- substr(SNP.barcode, 1, 16)
    lihc.barcode <- lihc.barcode[which(lihc.barcode %in% SNP.barcode)]

    sampleID <- substr(lihc.barcode, 1, 12)
    pairedID <- sampleID[which(duplicated(sampleID))]
    lihc.barcode <- lihc.barcode[which(sampleID %in% pairedID)]

    length(pairedID)        #   [1] 39
    length(lihc.barcode)    #   [1] 78

    #   filter by exome sequencing samples
    #   
    exome.barcode<- paste(lihc.exome.query$barcode, collapse=",")
    exome.barcode <- strsplit(exome.barcode, split=",")[[1]]
    exome.barcode <- substr(exome.barcode, 1, 16)
    lihc.barcode <- lihc.barcode[which(lihc.barcode %in% exome.barcode)]

    sampleID <- substr(lihc.barcode, 1, 12)
    pairedID <- sampleID[which(duplicated(sampleID))]
    lihc.barcode <- lihc.barcode[which(sampleID %in% pairedID)]

    length(pairedID)        #   [1] 28
    length(lihc.barcode)    #   [1] 56


    #   1.3.    Information of paired tumor/normal samples included 
    #           in all data sets
    #   
    sampleID <- substr(lihc.barcode, 1, 12)
    pairedID <- sampleID[which(duplicated(sampleID))]

    common.samples <- lihc.samples[which(lihc.samples$sample_ID %in% pairedID),]
    query.samples <- as.character(common.samples$barcode)

    head(common.samples)

    #      sample_ID          barcode         tissue_type tissue_ID
    # 2 TCGA-BC-A10Q TCGA-BC-A10Q-01A       Primary Tumor         1
    # 3 TCGA-BC-A10Q TCGA-BC-A10Q-11A Solid Tissue Normal        11
    # 4 TCGA-BC-A10R TCGA-BC-A10R-01A       Primary Tumor         1
    # 5 TCGA-BC-A10R TCGA-BC-A10R-11A Solid Tissue Normal        11
    # 8 TCGA-BC-A10T TCGA-BC-A10T-01A       Primary Tumor         1
    # 9 TCGA-BC-A10T TCGA-BC-A10T-11A Solid Tissue Normal        11

    head(query.samples)

    # [1] "TCGA-BC-A10Q-01A" "TCGA-BC-A10Q-11A" 
    # [3] "TCGA-BC-A10R-01A" "TCGA-BC-A10R-11A"
    # [5] "TCGA-BC-A10T-01A" "TCGA-BC-A10T-11A"

    fileBaseName <- paste0(filePath,"TCGA.LIHC.Paired.Samples.For.All.")
    save(common.samples, file=paste0(fileBaseName,"RData"))
    write.table(common.samples, sep="\t", quote=FALSE, row.names=FALSE, 
        col.names=TRUE, file=paste0(fileBaseName,"txt"))




#   2.  Download dataset 
#   ========================================================================

    fileBaseName <- paste0(filePath, "TCGAbiolinks.LIHC.");
    
    #   2.1     RNAseqV2 data   
    #   
    RNAseq.sampleID  <- strsplit(lihc.RNAseq.query$barcode, split=",")[[1]]
    RNAseq.barcodes  <- substr(RNAseq.sampleID, 1, 16)
    download.samples <- RNAseq.sampleID[RNAseq.barcodes %in% query.samples]
    length(download.samples)    #   [1] 56

    #   Raw read count only
    #
    TCGAdownload(lihc.RNAseq.query, samples=download.samples, path="RNAseqV2", 
                type="rsem.genes.results")

    lihc.RNAseq.data <- TCGAprepare(query=lihc.RNAseq.query, 
        dir="RNAseqV2", summarizedExperiment=FALSE,
        samples=download.samples, type="rsem.genes.results")
    
    save(lihc.RNAseq.data, file=paste0(fileBaseName,"RNAseqV2.Level_3.RData"))

    columns <- grep("TCGA", colnames(lihc.RNAseq.data))
    TCGA.LIHC.RNAseqV2.Raw.Reads <- lihc.RNAseq.data[, columns]
    save(TCGA.LIHC.RNAseqV2.Raw.Reads, 
        file=paste0(fileBaseName,"RNAseqV2.Raw.Counts.RData"))
    write.table(TCGA.LIHC.RNAseqV2.Raw.Reads, sep="\t", quote=FALSE,
            row.names=TRUE, col.names=TRUE, 
            file=paste0(fileBaseName,"RNAseqV2.Raw.Counts.txt"))

    unlink("RNAseqV2", recursive=TRUE)



    #   2.2     miRNAseq data   
    #   
    miRNAseq.sampleID  <- strsplit(lihc.miRNAseq.query$barcode, split=",")[[1]]
    miRNAseq.barcodes  <- substr(miRNAseq.sampleID, 1, 16)
    download.samples <- miRNAseq.sampleID[miRNAseq.barcodes %in% query.samples]
    
    length(download.samples)                            #   [1] 57
    which(duplicated(substr(download.samples, 1, 16)))  #   [1] 57
    download.samples <- download.samples[-57]

    TCGAdownload(lihc.miRNAseq.query, samples=download.samples, path="miRNAseq", 
                    type="13.mirna.quantification")
    lihc.miRNAseq.data <- TCGAprepare(query=lihc.miRNAseq.query,
        dir="miRNAseq", summarizedExperiment=FALSE, 
        samples=download.samples, type="mirna")
        
    save(lihc.miRNAseq.data,file=paste0(fileBaseName,"miRNAseqV2.Level_3.RData"))

    columns <- grep("read_count", colnames(lihc.miRNAseq.data))
    TCGA.LIHC.miRNA.Raw.Reads <- lihc.miRNAseq.data[,columns]
    colnames(TCGA.LIHC.miRNA.Raw.Reads) <- gsub(".{1,1}read_count", "", 
            colnames(TCGA.LIHC.miRNA.Raw.Reads))
    save(TCGA.LIHC.miRNA.Raw.Reads, 
            file=paste0(fileBaseName,"miRNAseqV2.Raw.Counts.RData"))
    write.table(TCGA.LIHC.miRNA.Raw.Reads, sep="\t", 
        file=paste0(fileBaseName,"miRNAseqV2.Raw.Counts.txt"),
        quote=FALSE, row.names=TRUE, col.names=TRUE)

    unlink("miRNAseq", recursive=TRUE)



    #   2.3     Methylation data. There are multiple list of barcodes
    #   
    methyl.sampleID <- paste(lihc.methyl.query$barcode, collapse=",")
    methyl.sampleID  <- strsplit(methyl.sampleID, split=",")[[1]]
    methyl.barcodes  <- substr(methyl.sampleID, 1, 16)
    download.samples <- methyl.sampleID[methyl.barcodes %in% query.samples]
    length(download.samples)    #   [1] 56

    TCGAdownload(lihc.methyl.query, path="DNA_Methylation", 
        samples=download.samples, type="HumanMethylation450")

    lihc.methylation.data <- TCGAprepare(query=lihc.methyl.query, 
        dir="DNA_Methylation", summarizedExperiment=FALSE,
        samples=download.samples, type="HumanMethylation450")
    save(lihc.methylation.data, 
        file=paste0(fileBaseName, "Methylation.Level_3.RData"))

    save(lihc.methylation.data, 
        file=paste0(fileBaseName, "Methylation.beta.Values.RData"))
    write.table(lihc.methylation.data, sep="\t",
        quote=FALSE, row.names=TRUE, col.names=TRUE,
        file=paste0(fileBaseName, "Methylation.beta.Values.txt"))

    unlink("DNA_Methylation", recursive=TRUE)



    #   2.4     SNP/CNV data lihc.SNP.query. Need extra procedures to  
    #           convert to different format later.
  
    SNP.sampleID <- paste(lihc.SNP.query$barcode, collapse=",")
    SNP.sampleID  <- strsplit(SNP.sampleID, split=",")[[1]]
    SNP.barcodes  <- substr(SNP.sampleID, 1, 16)
    download.samples <- SNP.sampleID[SNP.barcodes %in% query.samples]
    length(download.samples)    #   [1] 56

    TCGAdownload(lihc.SNP.query, path="CNV_SNP_Array", 
        samples=download.samples, type="hg19.seg")
        
    lihc.CNV_SNP.data <-TCGAprepare(query=lihc.SNP.query,
        dir="CNV_SNP_Array", summarizedExperiment=FALSE,
        samples=download.samples, type="hg19.seg")
    save(lihc.CNV_SNP.data, file=paste0(fileBaseName, "CNV_SNP.Level_3.RData"))

    save(lihc.CNV_SNP.data,file=paste0(fileBaseName,"CNV_SNP.seg_mean.RData"))
    write.table(lihc.CNV_SNP.data, sep="\t", quote=FALSE, 
        file=paste0(fileBaseName,"CCNV_SNP.seg_mean.txt"),
        row.names=TRUE, col.names=TRUE)

    unlink("CNV_SNP_Array", recursive=TRUE)
    unlink("mages", recursive=TRUE)



    #   2.5     Exome/Mutation data. It is in maf format and no prepare
    #           method available. Simply copy it to target directory and
    #           covert to RData objects for later use.
    #   
    TCGAdownload(lihc.exome.query, path="Mutation", 
        type="IlluminaGA_DNASeq.1.somatic.maf")

    mafFile <- list.files(path="Mutation/", pattern="maf", recursive=TRUE, 
                full.names=TRUE)
    lihc.Mutation.data <- read.table(mafFile, sep="\t", quote="", header=TRUE)
    save(lihc.Mutation.data, 
        file=paste0(fileBaseName,"Mutation.Level_2.MAF.RData"))
    
    save(lihc.Mutation.data, file=paste0(fileBaseName,"Mutation.Data.RData"))
    file.copy(from=mafFile, to=paste0(fileBaseName,"Mutation.Data.txt"))

    unlink("Mutation", recursive=TRUE)



#   End of TCGA.LiverHCC.Data.Download.R
#   =========================================================================



    