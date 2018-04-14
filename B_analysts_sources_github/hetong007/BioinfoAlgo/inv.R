txt = readLines('rosalind_ms.txt')
a = as.numeric(unlist(strsplit(txt[2],' ')))
n = length(a)

merge = function(a)
{
  n = length(a)
  if (n==1)
    return(a)
  m = n%/%2
  s = merge(a[1:m])
  t = merge(a[(m+1):n])
  i = 1
  j = 1
  l = 1
  sum = 0
  ans = integer(n)
  
  while(l<=n)
  {
    ans[l] = min(s[i],t[j])
    while (i<=m && j<=(n-m) && s[i]<=t[j])
      i = i+1
    if (i>m)
      ans = c(ans[1:l],t[j:(n-m)])
    while (i<=m && j<=(n-m) && s[i]>t[j])
    {
      j = j+1
      sum = sum+1
    }
    if (j>(n-m))
      ans = c(ans[1:l],s[i:m])
    l = l+1
  }
  return(ans)
}

merge(a)
