txt = readLines('rosalind_1b.txt')

txt = unlist(strsplit(txt,''))
txt = sapply(txt,function(x){
    if (x=='A')
        return('T')
    if (x=='T')
        return('A')
    if (x=='C')
        return('G')
    if (x=='G')
        return('C')
})

ans = paste(rev(txt),collapse='')
writeLines(ans,'1b.txt')
