txt = readLines('rosalind_ins.txt')
n = as.numeric(txt[1])
a = as.numeric(unlist(strsplit(txt[2],' ')))

ans = 0
for (i in 2:n)
{
  k = i
  while (k>1 && a[k]<a[k-1])
  {
    ans = ans+1
    tmp = a[k-1]
    a[k-1] = a[k]
    a[k] = tmp
    k = k-1
  }
}
writeLines(as.character(ans),'ins.txt')
