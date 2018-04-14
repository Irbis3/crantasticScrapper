prefixTable = function(pattern)
{
    if (length(pattern)==1 && nchar(pattern)>1)
        pattern = unlist(strsplit(pattern,''))
    m = length(pattern)
    phi = integer(m)
    phi[1] = 0
    k = 0
    for (q in 2:m)
    {
        while (k>0 && pattern[k+1]!=pattern[q])
            k = phi[k]
        if (pattern[k+1]==pattern[q])
            k = k+1
        phi[q] = k
    }
    return(phi)
}

KMP = function(pattern, string, phi = NULL)
{
    if (length(pattern)==1 && nchar(pattern)>1)
        pattern = unlist(strsplit(pattern,''))
    if (length(string)==1 && nchar(string)>1)
        string = unlist(strsplit(string,''))
    if (is.null(phi))
        phi = prefixTable(pattern)
    
    m = length(pattern)
    n = length(string)
    q = 0
    l = 1
    positions = NULL
    
    for (i in 1:n)
    {
        while (q>0 && pattern[q+1]!=string[i])
            q = phi[q]
        if (pattern[q+1]==string[i])
            q = q+1
        if (q==m)
        {
            positions[l] = i-m+1
            l = l+1
            q = phi[q]
        }
    }
    return(positions)
}