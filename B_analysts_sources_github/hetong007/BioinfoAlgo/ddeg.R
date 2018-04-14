txt = read.table('rosalind_ddeg.txt')
v = txt[1,1]
e = txt[1,2]
txt = txt[-1,]

mat = matrix(0,v,v)
for (e in 1:nrow(txt))
{
  i = txt[e,1]
  j = txt[e,2]
  mat[i,j] = 1
  mat[j,i] = 1
}

nb = rowSums(mat)
ans = integer(v)

for (i in 1:v)
{
  j = which(mat[i,]==1)
  ans[i] = sum(nb[j])
}

ans = paste(ans, collapse = ' ')
writeLines(ans,'ddeg.txt')
