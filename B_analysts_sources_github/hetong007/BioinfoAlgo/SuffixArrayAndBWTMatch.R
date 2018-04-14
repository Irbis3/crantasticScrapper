suffixArray = function(w,ending='$')
{
    n = nchar(w)
    if (substr(w,n,n)!=ending)
        w = paste0(w,ending)
    n = nchar(w)
    suffices = substring(w,1:n,rep(n,n))
    ind = order(suffices)
    ans = list(ind,suffices[ind])
    return(ans)
}

BWT = function(w,ending = '$')
{
    n = nchar(w)
    if (substr(w,n,n)!=ending)
        w = paste0(w,ending)
    n = nchar(w)
    sarr = suffixArray(w)[[1]]
    bwt = sarr-1
    bwt[which(bwt==0)] = n
    ans = list(bwt,substring(w,bwt,bwt))
    return(ans)
}

reverseBWT = function(bwt)
{
    if (length(bwt)==1 && nchar(bwt)>1)
        bwt = unlist(strsplit(bwt,''))
    n = length(bwt)
    sarr = sort(bwt)
    dict = unique(sarr)
    C = c(0,cumsum(table(sarr)))
    C = C[-length(C)]
    names(C) = dict
    occ = matrix(0,length(bwt),length(dict))
    ind = cbind(1:length(bwt),match(bwt,dict))
    occ[ind] = 1
    occ = apply(occ,2,cumsum)
    colnames(occ) = dict
    rownames(occ) = bwt
    
    origin = rep('',n)
    i = 2
    ind = 1
    
    origin[1] = '$'
    origin[2] = bwt[1]
    symbol = bwt[1]
    while(i<=n)
    {
        ind = C[symbol]+occ[ind,symbol]
        origin[i+1] = bwt[ind]
        symbol = bwt[ind]
        i = i+1
    }
    origin = rev(origin)[-1]
    origin
}

matchBWT = function(pattern, bwt, sarr = NULL, dict=NULL, C=NULL, occ=NULL)
{
    n = length(bwt)
    if (is.null(sarr))
        sarr = sort(bwt)
    if (is.null(dict))
        dict = unique(sarr)
    if (is.null(C))
    {
        C = c(0,cumsum(table(sarr)))
        C = C[-length(C)]
        names(C) = dict
    }
    if (is.null(occ))
    {
        occ = matrix(0,length(bwt),length(dict))
        ind = cbind(1:length(bwt),match(bwt,dict))
        occ[ind] = 1
        occ = apply(occ,2,cumsum)
        colnames(occ) = dict
        rownames(occ) = bwt
    }
    
    pattern = unlist(strsplit(pattern,''))
    m = length(pattern)
    pattern = rev(pattern)
    pattern = match(pattern,dict)
    
    l = 1
    r = n
    i = 1
    while(l<=r && i<=m)
    {
         a = pattern[i]
         if (l==1)
         {
             l = C[a]+1
         } else {
            l = C[a]+occ[l-1,a]+1
         }
         r = C[a]+occ[r,a]
         i = i+1
    }
    if (l>r)
        return(c(0,0))
    return(c(l,r))
}

ExactMatch = function(origin=NULL,bwt=NULL,patterns)
{
    if (is.null(bwt) && is.null(origin))
        stop('No Input')
    if (is.null(bwt))
        bwt = BWT(origin)[[2]]
    if (is.null(origin))
        if (length(bwt)==1 && nchar(bwt)>1)
            bwt = unlist(strsplit(bwt,''))
    #sarr = sort(bwt)
    
    sarr = suffixArray(origin)
    #ans = list()
    
    dict = sort(unique(bwt))

    C = c(0,cumsum(table(bwt)))
    C = C[-length(C)]
    names(C) = dict

    occ = matrix(0,length(bwt),length(dict))
    ind = cbind(1:length(bwt),match(bwt,dict))
    occ[ind] = 1
    occ = apply(occ,2,cumsum)
    colnames(occ) = dict
    rownames(occ) = bwt
    
    #ans = rep(0,length(patterns))
    ans = list()
    i = 1
    for (pattern in patterns)
    {
        cat(i,'\n')
        ind = matchBWT(pattern,bwt,sarr[[2]],dict,C,occ)
        if (ind[1]!=0 && ind[2]!=0)
            ans[[i]] = sort(sarr[[1]][ind[1]:ind[2]])#ans[i] = ind[2]-ind[1]+1#ans[[i]] = length(ind[1]:ind[2])#ans[[i]] = sort(sarr[[1]][ind[1]:ind[2]])
        else
            ans[[i]] = 0#ans[i] = 0
        i = i+1
    }
    return(ans)
}
































