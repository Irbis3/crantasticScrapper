alignment = function(v,w,type='global')
{
    #v = 'GTAGGCTTAAGGTTA'
    v = unlist(strsplit(v,''))
    #w = 'GTAGGCTTAAGGTTA'
    w = unlist(strsplit(w,''))
    v = c('-',v)
    w = c('-',w)
    n = length(v)
    m = length(w)
    score = function(a,b)
    {
        if (a==b)
            return(1)
        return(-1)
    }

    V = matrix(rep(0,m*n),nrow=n)
    V[1,1] = 0
    for (i in 2:n)
        V[i,1] = score(v[i],w[1])+V[i-1,1]
    for (j in 2:m)
        V[1,j] = score(v[1],w[j])+V[1,j-1]
    for (i in 2:n)
        for (j in 2:m)
        {
            V[i,j] = max(V[i-1,j-1]+score(v[i],w[j]),
                         V[i,j-1]+score('_',w[j]),
                         V[i-1,j]+score(v[i],'_'))
            if (type=='local')
                V[i,j] = max(0,V[i,j])
        }
    rownames(V) = v
    colnames(V) = w
    V
}

