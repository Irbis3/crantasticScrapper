library(Homo.sapiens)

genes = selectRangesById(Homo.sapiens, 'A1BG', columns=c('SYMBOL', 'TXID'), keytype='SYMBOL', overlaps='tx')

exby = exonsBy(Homo.sapiens, by='tx')

exons = exby[unlist(mcols(genes)$TXID)]
