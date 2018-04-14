txt = readLines('rosalind_bins.txt')
n = as.numeric(txt[1])
m = as.numeric(txt[2])
a = as.numeric(unlist(strsplit(txt[3],' ')))
query = as.numeric(unlist(strsplit(txt[4],' ')))

ans = NULL
ind = 1
for (q in query)
{
  l = 1
  r = n
  while (l<=r)
  {
    mid = (l+r)%/%2
    #cat(l,mid,r,'\n')
    if (a[mid]==q)
    {
      ans[ind] = mid
      ind = ind+1
      break;
    }
    else if (a[mid]<q)
    {
      l = mid+1
    }
    else
      r = mid-1
  }
  if (l>r)
  {
    ans[ind] = -1
    ind = ind+1
  }
}

ans = paste(ans,collapse = ' ')
writeLines(ans,'bins.txt')
