#' Genome Initialization
#' 
#' \code{initializeGenomeObject} initializes the Rcpp Genome object
#' 
#' @param file A file of coding sequences in fasta or RFPData format
#' 
#' @param genome A genome object can be passed in to concatenate the input file to it (optional).
#' 
#' @param observed.expression.file A string containing the location of a file containing
#'  empirical expression rates (optional).
#' 
#' @param fasta A boolean value which decides whether to initialize with a
#'  fasta file or an RFPData file. (TRUE for fasta, FALSE for RFPData)
#' 
#' @param match.expression.by.id If TRUE (default), observed expression values will be assigned by matching sequence identifier.
#' If FALSE, observed expression values will be assigned by order.
#' 
#' @param append If TRUE (FALSE is default), function will read in additional genome data to append to an existing genome.
#' If FALSE, genome data is cleared before reading in data (no preexisting data). 
#' 
#' @return This function returns the initialized Genome object.
#' 
#' @examples 
#' 
#' genome_file <- system.file("extdata", "genome.fasta", package = "AnaCoDa")
#' genes_file <- system.file("extdata", "more_genes.fasta", package = "AnaCoDa")
#' expression_file <- system.file("extdata", "expression.csv", package = "AnaCoDa")
#'  
#' ## reading genome
#' genome <- initializeGenomeObject(file = genome_file)
#' 
#' ## reading genome and observed expression data
#' genome <- initializeGenomeObject(file = genome_file, observed.expression.file = expression_file)
#'  
#' ## add aditional genes to existing genome
#' genome <- initializeGenomeObject(file = genome_file)
#' genome <- initializeGenomeObject(file = genes_file, genome = genome, append = TRUE)   
#' 
initializeGenomeObject <- function(file, genome=NULL, observed.expression.file=NULL, fasta=TRUE, match.expression.by.id=TRUE, append=FALSE) {
  if (is.null(genome)){ 
    genome <- new(Genome)
  }

  if (fasta == TRUE) {
    genome$readFasta(file, append)
  } else {
    genome$readRFPData(file, append)
  }
  if(!is.null(observed.expression.file)) {
    genome$readObservedPhiValues(observed.expression.file, match.expression.by.id)
  }
  return(genome)
}


#' Get Codon Counts For Each Amino Acid 
#' 
#' @param aa A one character representation of an amino acid.
#' 
#' @param genome A genome object from which the counts of each
#' codon can be obtained.
#'  
#' @return Returns a matrix storing the codonCounts for the given amino acid. 
#' 
#' @description provides the codon counts for a fiven amino acid across all genes
#' 
#' @details The returned matrix containes a row for each gene and a coloumn 
#' for each synonymous codon of \code{aa}.
#' 
#' @examples 
#' 
#' genome_file <- system.file("extdata", "genome.fasta", package = "AnaCoDa")
#'  
#' ## reading genome
#' genome <- initializeGenomeObject(file = genome_file)
#' countsForA <- getCodonCountsForAA("A", genome)
#' 
#' counts <- lapply(X = c("A", "C"), FUN = getCodonCountsForAA, genome = genome)
#' 
getCodonCountsForAA <- function(aa, genome){
  # get codon count for aa
  codons <- AAToCodon(aa, F)
  codonCounts <- lapply(codons, function(codon){
    codonCounts <- genome$getCodonCountsPerGene(codon)
  })
  codonCounts <- do.call("cbind", codonCounts)
  return(codonCounts)
}



#' Length of Genome
#' 
#' \code{length} gives the length of a genome
#' 
#' @param x A genome object initialized with \code{\link{initializeGenomeObject}}.
#' 
#' @return returns the number of genes in a genome
#' 
#' @examples 
#' 
#' genome_file <- system.file("extdata", "genome.fasta", package = "AnaCoDa")
#'  
#' ## reading genome
#' genome <- initializeGenomeObject(file = genome_file)
#' length(genome) # 10
#' 
length.Rcpp_Genome <- function(x) {
  return(x$getGenomeSize(F))
}

#' Summary of Genome
#' 
#' \code{summary} summarizes the description of a genome, such as number of genes and average gene length.
#' 
#' @param object A genome object initialized with \code{\link{initializeGenomeObject}}.
#' 
#' @param ... Optional, additional arguments to be passed to the main summary function 
#' that affect the summary produced.
#'
#' @return This function returns by default an object of class c("summaryDefault", table").
summary.Rcpp_Genome <- function(object, ...) {
  # TODO output stuff like:
  # - no. of genes
  # - avg. gene length
  # - avg. A,C,G,T content
  # - avg. AA composition
  # - ...
  summary(object, ...)
}

#' Gene Names of Genome
#' 
#' 
#' @param genome A genome object initialized with \code{\link{initializeGenomeObject}}.
#' 
#' @param simulated A logical value denoting if the gene names to be listed are simulated or not.
#' The default value is FALSE.
#' 
#' @description returns the identifiers of the genes within the genome specified.
#' 
#' @return gene.names Returns the names of the genes as a vector of strings.
#' 
#' @examples 
#' 
#' genome_file <- system.file("extdata", "genome.fasta", package = "AnaCoDa")
#'  
#' ## reading genome
#' genome <- initializeGenomeObject(file = genome_file)
#'
#' ## return all gene ids for the genome
#' geneIDs <- getNames(genome, FALSE)
#' 
getNames <- function(genome, simulated = FALSE)
{
  genes <- genome$getGenes(simulated)
  gene.names <- unlist(lapply(1:length(genes), function(i){return(genes[[i]]$id)}))
  return(gene.names)
}


#' Add gene observed synthesis rates
#' 
#' \code{addObservedSynthesisRateSet} returns the observed 
#' synthesis rates of the genes within the genome specified.
#' 
#' @param genome A genome object initialized with 
#' \code{\link{initializeGenomeObject}} to add observed expression data.
#' 
#' @param observed.expression.file A string containing 
#' the location of a file containing empirical expression rates (optional).
#' 
#' @param match.expression.by.id If TRUE (default) observed expression 
#' values will be assigned by matching sequence identifier.
#' If FALSE observed expression values will be assigned by order
#' 
#' @return Returns the genome after adding the new gene expression values
#' 
#' @examples 
#' 
#' genome_file <- system.file("extdata", "genome.fasta", package = "AnaCoDa")
#' expression_file <- system.file("extdata", "expression.csv", package = "AnaCoDa") 
#' ## reading genome
#' genome <- initializeGenomeObject(file = genome_file)
#' 
#'
#' ## add expression values after the genome was initiallized, 
#' ## or adding an additional set of expression values
#' genome <- addObservedSynthesisRateSet(genome = genome, 
#'                    observed.expression.file = expression_file)
#' 
addObservedSynthesisRateSet <- function(genome, observed.expression.file, match.expression.by.id=TRUE)
{
  genome$readObservedPhiValues(observed.expression.file, match.expression.by.id)
  return(genome)
}


#' Get gene observed synthesis rates
#' 
#' \code{getObservedSynthesisRateSet} returns the observed 
#' synthesis rates of the genes within the genome specified.
#' 
#' @param genome A genome object initialized with \code{\link{initializeGenomeObject}}.
#' 
#' @param simulated A logical value denoting if the synthesis 
#' rates to be listed are simulated or not. The default value is FALSE.
#' 
#' @return Returns a data.frame with the observed expression values in genome
#' 
#' @examples 
#' 
#' genome_file <- system.file("extdata", "genome.fasta", package = "AnaCoDa")
#' expression_file <- system.file("extdata", "expression.csv", package = "AnaCoDa") 
#' ## reading genome
#' genome <- initializeGenomeObject(file = genome_file)
#' 
#'
#' ## return expression values as a data.frame with gene ids in the first column.
#' expressionValues <- getObservedSynthesisRateSet(genome = genome)
#' 
getObservedSynthesisRateSet <- function(genome, simulated = FALSE)
{
  genes <- genome$getGenes(simulated)
  expression <- lapply(1:length(genes), function(i){return(genes[[i]]$getObservedSynthesisRateValues())})
  ids <- getNames(genome, simulated)
  mat <- do.call(rbind,expression)
  return(cbind.data.frame(ids, mat,stringsAsFactors=F))
}

#' Calculate the CAI codon weigths for a reference genome
#' 
#' 
#' \code{getCAIweights} returns the weights for the Codon Adaptation Index
#' based on a reference genome.
#' 
#' @param referenceGenome A genome object initialized with \code{\link{initializeGenomeObject}}.
#' 
#' @return Returns a named vector with the CAI weights for each codon
#' 
#' @examples 
#' 
#' genome_file <- system.file("extdata", "genome.fasta", package = "AnaCoDa")
#'
#' ## reading genome
#' referenceGenome <- initializeGenomeObject(file = genome_file)
#' 
#' wi <- getCAIweights(referenceGenome)
#' 
getCAIweights <- function(referenceGenome)
{
  aa.vec <- aminoAcids()
  aa.vec <- aa.vec[-length(aa.vec)]
  
  wi.list <- vector(mode = "list", length = length(aa.vec))
  names(wi.list) <- aa.vec
  
  codon.names <- NULL
  for(aa in aa.vec)
  {
    codon.names <- c(codon.names, AAToCodon(aa))
    ## create reference table for each codon and gene
    codonCountForAA.ref <- getCodonCountsForAA(aa, genome = referenceGenome)
    fi <- colSums(codonCountForAA.ref)
    fi.max <- max(fi)
    wi.list[[aa]] <- fi / fi.max
  }
  
  wi.vec <- unlist(wi.list)
  names(wi.vec) <- codon.names
  return(wi.vec)
}


### NOT EXPOSED
calcCAI <- function(gene, wi)
{
  # sequence string to triplets
  seq <- gene$seq
  seq <- unlist(strsplit(seq, ""))
  seq <- paste(seq[c(T,F,F)], seq[c(F,T,F)], seq[c(F,F,T)], sep="")
  codon.length <- length(seq)
  
  CAI <- 1
  for(s in seq)
  {
    if(is.na(wi[s])) next
    CAI <- CAI * wi[s]
  }
  CAI <- CAI^(1/codon.length)
  return(CAI)
}

#' Calculate the Codon Adaptation Index 
#' 
#' 
#' \code{getCAI} returns the Codon Adaptation Index for a 
#' genome based on a provided reference.
#' 
#' @param referenceGenome A genome object initialized with \code{\link{initializeGenomeObject}}.
#' Serves as reference set to calculate the necessary codon weights.
#' 
#' @param testGenome A genome object initialized with \code{\link{initializeGenomeObject}}.
#' The genome for which the CAI is supposed to be calculated
#' 
#' @return Returns a named vector with the CAI for each gene
#' 
#' @examples 
#' 
#' genome_file1 <- system.file("extdata", "more_genes.fasta", package = "AnaCoDa")
#' genome_file2 <- system.file("extdata", "genome.fasta", package = "AnaCoDa")
#'
#' ## reading genome
#' referenceGenome <- initializeGenomeObject(file = genome_file1)
#' testGenome <- initializeGenomeObject(file = genome_file2)
#'
#' cai <- getCAI(referenceGenome, testGenome)
#' 
getCAI <- function(referenceGenome, testGenome)
{
  genes <- testGenome$getGenes(FALSE)
  wi <- getCAIweights(referenceGenome)
  CAI <- unlist(lapply(genes, calcCAI, wi))
  names(CAI) <- getNames(testGenome, FALSE)
  return(CAI)  
}


#' Calculate the Effective Number of Codons
#' 
#' 
#' \code{getNc} returns the Effective Number of Codons for a genome.
#' 
#' @param genome A genome object initialized with \code{\link{initializeGenomeObject}}.
#' 
#' @return Returns a named vector with the Effective Number of Codons for each gene
#' 
#' @examples 
#' 
#' genome_file <- system.file("extdata", "more_genes.fasta", package = "AnaCoDa")
#' ## reading genome
#' genome <- initializeGenomeObject(file = genome_file)
#'
#' nc <- getNc(genome)
#' 
getNc <- function(genome)
{
  aa.vec <- aminoAcids()
  aa.vec <- aa.vec[-length(aa.vec)]
  
  f.mat <- matrix(0, ncol = 6, nrow = length(genome))
  division.counter <- matrix(0, ncol = 6, nrow = length(genome))
  
  for(aa in aa.vec)
  {
    if(aa == "M" || aa == "W") next # contribution of M and W is 2 in total
    
    codonCountForAA <- getCodonCountsForAA(aa, genome = genome)
    n <- rowSums(codonCountForAA)
    pi <- codonCountForAA / n
    
    f.vec <- ( ((n*rowSums(pi*pi))-1) / (n-1) )
    f.vec[!is.finite(f.vec)] <- 0
    
    ncodons <- length(AAToCodon(aa))
    f.mat[, ncodons] <- f.mat[, ncodons] + f.vec
    division.counter[n > 1, ncodons] <- division.counter[n > 1, ncodons] + 1
  }
  
  # adjusted number of AA with codons 2, 4, and 6 since we split Serine
  meanF <- data.frame(SF2=f.mat[,2]/division.counter[, 2], SF3=f.mat[,3], SF4=f.mat[,4]/division.counter[, 4], SF6=f.mat[,6]/division.counter[, 6])
  rare.Ile <- meanF$SF3 < 1
  meanF$SF3[rare.Ile] <- (meanF$SF2[rare.Ile] + meanF$SF4[rare.Ile])/2 # correcting for rare or mising Ile as suggested by Wright (1990, p25)
  
  #Wright (1990) Eqn. 3 adjusted for split serine
  Nc <- 2 + 10/meanF$SF2 + 1/meanF$SF3 + 6/meanF$SF4 + 2/meanF$SF6
  Nc[Nc > 61] <- 61 # revising Nc as suggested by Wright (1990, p25)
  names(Nc) <- getNames(genome, FALSE)
  return(Nc)  
}

#' Calculate the Effective Number of Codons for each Amino Acid
#' 
#' 
#' \code{getNcAA} returns the Effective Number of Codons for each Amino Acid.
#' 
#' @param genome A genome object initialized with \code{\link{initializeGenomeObject}}.
#' 
#' @return Returns an object of type \code{data.frame} with the Effective Number of Codons
#' for each amino acid in each gene.
#' 
#' @examples 
#' 
#' genome_file <- system.file("extdata", "more_genes.fasta", package = "AnaCoDa")
#' ## reading genome
#' genome <- initializeGenomeObject(file = genome_file)
#'
#' nc <- getNcAA(genome)
#' 
getNcAA <- function(genome)
{
  aa.vec <- aminoAcids()
  aa.vec <- aa.vec[-length(aa.vec)]
  
  f.mat <- data.frame(matrix(NA, ncol = length(aa.vec), nrow = length(genome)))
  colnames(f.mat) <- aa.vec
  for(aa in aa.vec)
  {
    if(aa == "M" || aa == "W") next # contribution of M and W is 2 in total
    
    codonCountForAA <- getCodonCountsForAA(aa, genome = genome)
    n <- rowSums(codonCountForAA)
    pi <- codonCountForAA / n
    
    f.vec <- ( ((n*rowSums(pi*pi))-1) / (n-1) )
    f.vec <- 1/f.vec
    f.vec[!is.finite(f.vec)] <- NA
    
    f.mat[[aa]] <- f.vec
  }
  rownames(f.mat) <- getNames(genome, FALSE)
  return(f.mat)
}