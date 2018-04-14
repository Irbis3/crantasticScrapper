#source('ConnectFour.R')

ParameterInitializer = function(layers)
{
    n = length(layers)
    W = vector(n-1,mode='list')
    for (i in 1:(n-1))
        W[[i]] = matrix(rnorm(layers[i]*layers[i+1],mean=0,sd=0.1),
                        ncol=layers[i])
    W
}

ForwardPropagation = function(cf,W,output='all')
{
    f = function(z) 1/(1+exp(-z))
    
    if (is.null(W))
        stop('Not enough input.')
    n = length(W)
    
    x = feature(cf)

    a = vector(n+1,mode='list')
    a[[1]] = x
    z = a

    for (i in 1:n)
    {
        z[[i+1]] = W[[i]]%*%a[[i]]
        a[[i+1]] = f(z[[i+1]])
    }
    res = a[[n+1]]
    return(res)
}

updateW = function(cf,W,alpha,beta,lambda,prev,Ending,result)
{
    w = W[[2]]#k*j
    v = W[[1]]#j*i
    e2 = prev$e2 
    e3 = prev$e3
    h = prev$h
    x = prev$x
    y = prev$y
    k = length(y)

    if (Ending)
    {
        #browser()
        if (result==1)
        {
            ny = c(1,0)
        }
        else if (result==-1)
        {
            ny = c(0,1)
        }
        else
        {
            ny = c(0.5,0.5)
        }
    }
    else
        ny = ForwardPropagation(cf,W)
    
    omega = ny-y
    #delta_v = list()
    
    for (i in 1:k)
    {
        delta_v = (w[i,]*h*(1-h))%*%t(x)
        delta_v = delta_v * (y[i]*(1-y[i]))
        e3[[i]] = lambda*e3[[i]]+delta_v
        v = v+alpha*omega[i]*e3[[i]]
    }
    
    delta_w = (y*(1-y))%*%t(h)
    e2 = lambda*e2+delta_w
    w = w+beta*diag(as.vector(omega))%*%e2
    
    x = feature(cf)
    h = 1/(1+exp(-v%*%x))
    W = list(v,w)
    prev = list(e2=e2,e3=e3,h=h,x=x,y=ny)
    return(list(W,prev))
}    
    