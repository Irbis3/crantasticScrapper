txt = readLines('rosalind_1a.txt')
k = as.numeric(txt[2])
txt = txt[1]

n = nchar(txt)
kmer = substring(txt,1:(n-k+1),k:n)
tb = sort(table(kmer),decreasing = TRUE)
mx = which(tb==max(tb))
ans = sort(names(mx))
ans = paste(ans,collapse=' ')
writeLines(ans,'1a.txt')
