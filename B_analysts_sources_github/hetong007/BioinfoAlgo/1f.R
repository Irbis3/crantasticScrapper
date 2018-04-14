txt = readLines('rosalind_1f.txt')
pattern = txt[1]
string = txt[2]
d = as.numeric(txt[3])

pattern = unlist(strsplit(pattern,''))
string = unlist(strsplit(string,''))
m = length(pattern)
n = length(string)

ans = NULL

for (i in 1:(n-m+1))
{
    kmer = string[i:(i+m-1)]
    if (sum(kmer!=pattern)<=d)
        ans = c(ans,i)
}

ans = ans-1

ans = paste(ans,collapse=' ')
writeLines(ans,'1f.txt')
