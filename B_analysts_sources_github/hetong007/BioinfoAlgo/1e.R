txt = readLines('rosalind_1e.txt')
txt = unlist(strsplit(txt,''))

n = length(txt)
ans = integer(n)

if (txt[1]=='C')
    ans[1] = -1
if (txt[1]=='G')
    ans[1] = 1

for (i in 2:n)
{
    if (txt[i]=='C')
        ans[i] = ans[i-1]-1
    else if (txt[i]=='G')
        ans[i] = ans[i-1]+1
    else
        ans[i] = ans[i-1]
}

ans = which(ans == min(ans))

ans = paste(ans,collapse=' ')
writeLines(ans,'1e.txt')
