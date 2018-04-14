trans2M = function (data, ulist = c(), ilist = c(), score, user.lt = NULL, user.ht = NULL, item.lt = NULL, item.ht = NULL) 
{
    user.id = unique(data[[1]])
    item.id = unique(data[[2]])
    item = sort(as.numeric(item.id))
    user = sort(as.numeric(user.id))
    M = setM(data, user, item, score)
    return(M)
}

setM = function (D, user, item, score) 
{
    M = Matrix(data = 0, nrow = length(user), ncol = length(item))
    uidx = match(D[[1]], user, nomatch = 0)
    sidx = match(D[[2]], item, nomatch = 0)
    i = uidx > 0 & sidx > 0
    M = as(M, "dgTMatrix")
    M@i = as.integer(uidx[i] - 1)
    M@j = as.integer(sidx[i] - 1)
    M@x = as.numeric(score[i])
    M = as(M, "dgCMatrix")
    return(M)
}

ForwardSub = function(L,y)
{
    n = nrow(L)
    dL = diag(L)
    x = numeric(n)
    x[1] = y[1]/dL[1]
    for (i in 2:n)
        x[i] = (y[i]-sum(L[i,]*x))/dL[i]
    return(x)
}

BackSub = function(U,y)
{
    n = nrow(U)
    dU = diag(U)
    x = numeric(n)
    x[n] = y[n]/dU[n]
    for (i in (n-1):1)
        x[i] = (y[i]-sum(U[i,]*x))/dU[i]
    return(x)
}

SolveSystem = function(mat)
{
    tmp = expand(lu(mat))
    P = tmp$P
    L = tmp$L
    U = tmp$U
    Q = tmp$Q
    n = nrow(mat)
    y = c(rep(0,n-1),1)

    y = P %*% as.matrix(y)
    x = ForwardSub(L,y)
    w = BackSub(U,x)
    w = t(Q) %*% as.matrix(w)
    return(w)
}

getScore = function(strat,p=0.37)
{
    N = length(strat)+1
    triple = rbind(c(1,1,1),c(N+1,N+1,1))
    triple = rbind(triple,cbind(2:N,(2:N)-strat,1-p))
    triple = rbind(triple,cbind(2:N,(2:N)+strat,p))
    triple = rbind(triple,cbind(2:N,2:N,-1))
    triple = as.data.frame(triple)

    mat = trans2M(triple,score=triple[[3]])

    #ans = solve(mat)[2:N,N+1]
    ans = SolveSystem(mat)
    return(ans)
}

getDivide = function(N,p=0.37)
{
    if (N%%2==1 || N<=4)
        return(getScore(getSeries(N))[2:N])
    pre = getDivide(N/2,p)
    pre = pre*p
    post = pre*(1-p)/p+p
    ans = c(pre,p,post)
    return(ans)
}

getAllPossible = function(N)
{
    x = 1:(N-1)
    y = N-x
    pos = pmin(x,y)
    m = prod(pos)
    ans = matrix(1,nrow=1)
    for (i in 2:(N-2))
    {
        tmp_n = nrow(ans)
        ans = do.call("rbind", rep(list(ans), pos[i]))
        tmp = rep(1:pos[i],e=tmp_n)
        ans = cbind(ans,tmp)
    }
    tmp_n = nrow(ans)
    ans = cbind(ans,rep(1,e=tmp_n))
    colnames(ans)=NULL
    return(ans)
}

getTest = function(N)
{
    strategy = getAllPossible(N)
    ans = apply(strategy,1,getScore)
    ind = which(ans==max(ans))
    if (length(ind)>1)
    {
        bid = apply(strategy[ind,],1,mean)
        ind_b = which(bid==min(bid))
        ind_b = ind[ind_b]
    }
    else
        ind_b = ind
    #find the minimum bid
    
    return(strategy[ind_b,])
}

getSeries = function(N)
{
    if (N<=8)
    {
        if (N==4)   
            return(c(1,2,1))
        if (N==5)
            return(c(1,2,2,1))
        if (N==6)
            return(c(1,1,3,1,1))
        if (N==7)
            return(c(1,2,3,3,2,1))
        return(c(1,2,1,4,1,2,1))
    }
    if (N%%2==1)
    {
        m = (N-1)/2
        return(c(1:m,m:1))
    }
    tmp = getSeries(N/2)
    ans = c(tmp,N/2,tmp)
    return(ans)
}

getAns = function(N)
{
    file_name = paste('n',N,'.csv',sep='')
    ans = getSeries(N)
    tmp = getDivide(N)
    tmp = sprintf('%.9f',tmp)
    ans = cbind(1:(N-1),ans,tmp)
    write.table(ans,file=file_name,row.names=F,col.names=F,sep=',',quote=F)
}

require(Matrix)
#getAns(5)
#getAns(1024)
#getAns(1000000)

getValid = function(wr,p=0.37)
{
    n = length(wr)+1
    ans = NULL
    for (i in 1:(n-1))
    {
        k = 1
        flag = FALSE
        while(!flag)
        {
            if (k==i || k==n-i)
            {
                flag = TRUE
                ans[i] = k
            }
            else if (abs(wr[i]-(1-p)*wr[i-k]-p*wr[i+k])<1e-5)
            {
                flag = TRUE
                ans[i] = k
            }
            k = k+1
        }
    }
    return(ans)
}
