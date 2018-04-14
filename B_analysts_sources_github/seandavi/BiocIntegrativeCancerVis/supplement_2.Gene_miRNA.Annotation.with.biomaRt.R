#
#   Gene and miRNA annotations with biomaRt package
#
#   References:
#   https://www.bioconductor.org/packages/release/bioc/html/biomaRt.html
#
#   Last updated on May 09, 2016 by Henry Zhang (hzhang@mail.nih.gov)
#   ________________________________________________________________________
#   <biomaRt><biomaRt><biomaRt><biomaRt><biomaRt><biomaRt><biomaRt><biomaRt>

    fileDir <- "TCGAbiolinks.Download/"

    library(biomaRt);
    mart <- useDataset("hsapiens_gene_ensembl", useMart("ensembl"));


    #   Gene annotation
    #   ===================================
    annot.data <- getBM(attributes=c(
                     "hgnc_symbol",
                    "chromosome_name",
                    "strand",
                    "start_position",
                    "end_position"),
                    mart=mart);


    #   Keep only chromosome 1~22, X and Y
    chromosomes <- c(1:22, "X", "Y")
    chromRows <- which(annot.data$chromosome_name %in% chromosomes)
    biomaRt.Gene.Annotation <- annot.data[chromRows,];

    #   Keep an unique gene set for convenience
    noGeneRows <- which(biomaRt.Gene.Annotation$hgnc_symbol=="")
    biomaRt.Gene.Annotation <- biomaRt.Gene.Annotation[-noGeneRows,]

    dupRows <- which(duplicated(biomaRt.Gene.Annotation$hgnc_symbol))
    biomaRt.Gene.Annotation <- biomaRt.Gene.Annotation[-dupRows,]

    biomaRt.Gene.Annotation$chromosome_name <- paste0("chr",
            biomaRt.Gene.Annotation$chromosome_name)
    geneOrder <- order(biomaRt.Gene.Annotation$hgnc_symbol)
    biomaRt.Gene.Annotation <- biomaRt.Gene.Annotation[geneOrder,]

    #   head(biomaRt.Gene.Annotation)
    #           hgnc_symbol chromosome_name strand start_position end_position
    #   35409        A1BG           chr19     -1       58345178     58353499
    #   40799    A1BG-AS1           chr19      1       58347751     58355183
    #   39607        A1CF           chr10     -1       50799409     50885675
    #   27787         A2M           chr12     -1        9067664      9116229
    #   27318     A2M-AS1           chr12      1        9065177      9068060
    #   39181       A2ML1           chr12      1        8822472      8887001
    
    save(biomaRt.Gene.Annotation, 
        file=paste0(fileDir,"biomaRt.Gene.Annotation.GRCh38.p5.RData"))
    write.table(biomaRt.Gene.Annotation, sep="\t", quote=FALSE,
            col.names=TRUE, row.names=FALSE, 
            file=paste0(fileDir,"biomaRt.Gene.AnnotationGRCh38.p5.txt"))


    #   miRNA annotation
    #   =====================================================
    annot.data <- getBM(attributes=c(
                    "mirbase_id",
                    "chromosome_name",
                    "strand",
                    "start_position",
                    "end_position"),
                    mart=mart);

    #   Keep only chromosome 1~22, X and Y 
    chromosomes <- c(1:22, "X", "Y")
    chromRows <- which(annot.data$chromosome_name %in% chromosomes)
    biomaRt.miRNA.Annotation <- annot.data[chromRows,];

    noMirRows <- which(biomaRt.miRNA.Annotation$mirbase_id=="")
    biomaRt.miRNA.Annotation <- biomaRt.miRNA.Annotation[-noMirRows,]

    dupRows <- which(duplicated(biomaRt.miRNA.Annotation$mirbase_id))
    biomaRt.miRNA.Annotation <- biomaRt.miRNA.Annotation[-dupRows,]
    
    biomaRt.miRNA.Annotation$chromosome_name <- paste0("chr",
            biomaRt.miRNA.Annotation$chromosome_name)
    mirnaOrder <- order(biomaRt.miRNA.Annotation$mirbase_id)
    biomaRt.miRNA.Annotation <- biomaRt.miRNA.Annotation[mirnaOrder,]

    #   head(biomaRt.miRNA.Annotation)
    #            mirbase_id chromosome_name strand start_position end_position
    #   8793   hsa-let-7a-2           chr11     -1      122146521    122146593
    #   9074     hsa-let-7b           chr22      1       46113686     46113768
    #   12060  hsa-let-7f-2            chrX     -1       53557192     53557274
    #   5638    hsa-mir-1-1           chr20      1       62554303     62554379
    #   6179    hsa-mir-100           chr11     -1      122152229    122152308
    #   4226  hsa-mir-101-1            chr1     -1       65058434     65058508

    save(biomaRt.miRNA.Annotation, 
        file=paste0(fileDir,"biomaRt.miRNA.Annotation.GRCh38.p5.RData"))
    write.table(biomaRt.miRNA.Annotation, sep="\t", 
            quote=FALSE, col.names=TRUE, row.names=FALSE, 
            file=paste0(fileDir,"biomaRt.miRNA.Annotation.txt"))



#   End of Annotation.with.biomaRt.R
#   =========================================================================