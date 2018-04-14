library(synapseClient)
# see ~/.synapseConfig file
synapseLogin()
# Get Mammomics project
proj = synGet('syn3519059')
# Folder for all data
allFolder = Folder(name='Omics Data',parentId=proj$properties$id)
allFolder = synStore(allFolder)

# Expression Data--Microarray
expFolder = Folder(name='Microarray Expression',parentId=allFolder$properties$id)
expFolder = synStore(expFolder)
expCelFolder = Folder(name='.Cel files',parentId=expFolder$properties$id)
expCelFolder = synStore(expCelFolder)
celfiles = dir('Mamomics.upload/transcriptome/',pattern='*\\.CEL\\.gz',full.names=TRUE)
for(i in celfiles) {
    celfile = File(path=i,parentId=expCelFolder$properties$id)
    synSetAnnotations(celfile) = list(type='CEL',assay='transcriptome')
    synStore(celfile)
    message(i)
}

# MouseDivGeno arrays
snpFolder = synStore(Folder(name='SNP Arrays',parentId=allFolder$properties$id))
snpCelFolder = synStore(Folder(name='.Cel files',parentId=snpFolder$properties$id))

celfiles = dir('Mamomics.upload/MouseDivData/',pattern='*\\.CEL\\.gz',full.names=TRUE)
for(i in celfiles) {
    celfile = File(path=i,parentId=snpCelFolder$properties$id)
    synSetAnnotations(celfile) = list(type='CEL',assay='snp',platform='Mouse Diversity Array')
    synStore(celfile)
    message(i)
}

# multisample VCF

variantFolder = synStore(Folder(name='DNA Sequencing Variants',parentId=allFolder$properties$id))
multisampVcfFolder   = synStore(Folder(name='multisample VCF files',parentId=variantFolder$properties$id))

# get both vcf.gz and .tbi files
vcffiles = dir('Mamomics.upload/exome',pattern='*vcf*',full.names=TRUE)
for(i in vcffiles) {
    vcffile = File(path=i,parentId=multisampVcfFolder$properties$id)
    if(grepl('tbi',i)) {
        synSetAnnotations(vcffile) = list(type='tbi',assay='exome',platform='Illumina')
    } else {
        synSetAnnotations(vcffile) = list(type='vcf',assay='exome',platform='Illumina')
    }
    synStore(vcffile)
    message(i)
}

