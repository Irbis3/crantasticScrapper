#source both files

playConnectFour = function(W1,W2=W1)
{
    W = list(W1,W2)
    cf = ConnectFour()
    record = list()
    counter = 0
    
    Ending = FALSE
    result = 0
    
    while(!Ending)
    {
        id = player(cf)
        moves = validStep(cf)
        mx = -1
        mxi = 0
        
        for (i in moves)
        {
            tbd = play(cf,i)
            #plyr = 1.5-player(cf)/2
            if (id==-1)
                plyr = 2
            else
                plyr = 1
            pred = ForwardPropagation(tbd,W[[plyr]])
            #pred = ForwardPropagation(tbd,W)
            #pred = pred[2-player(cf)]
            pred = pred[plyr]/sum(pred)
            if (pred>mx)
            {
                mx = pred
                mxi = i
            }
        }
        tbd = play(cf,mxi)
        
        result = win(tbd)
        if (result!=0 || all(board(tbd)!=0))
            Ending = TRUE
        
        counter = counter+1
        record[[counter]] = tbd
        cf = tbd
    }

    record
}

singleTrain = function(record,layers=c(86,50,2),W=NULL,
                           alpha=0.1,beta=0.1,lambda=0.1)
{
    w = W[[2]]
    v = W[[1]]
    if (length(record)>2)
    {
        
        x = feature(record[[1]])
        y = ForwardPropagation(record[[1]],W)
    }
    else
    {
        x = feature(record)
        y = ForwardPropagation(record,W)
    }
    h = 1/(1+exp(-v%*%x))
    k = length(y)
    e2 = (y*(1-y))%*%t(h)
    
    delta_v = list()
    e3 = vector(k,mode='list')
    for (i in 1:k)
    {
        delta_v = (w[i,]*h*(1-h))%*%t(x)
        e3[[i]] = delta_v * (y[i]*(1-y[i]))
    }
    
    prev = list(x=x,h=h,y=y,e2=e2,e3=e3)
    
    n = length(record)
    if (n>1)
        result = win(record[[n]])
    else
        result = win(record)
    
    if (n>1)
    {
        for (i in 2:(n-1))
        {
            upd = updateW(record[[i]],W,alpha,beta,lambda,prev,FALSE,0)
            W = upd[[1]]
            prev = upd[[2]]
        }
        upd = updateW(record[[n]],W,alpha,beta,lambda,prev,TRUE,result)
    }
    else
        upd = updateW(record,W,alpha,beta,lambda,prev,TRUE,result)
    
    W = upd[[1]]
    W
}

trainWeight = function(W=NULL, layers=c(86,50,2), time=100, 
                       random=FALSE, path=NULL, records=TRUE)
{
    if (is.null(W))
        W = ParameterInitializer(layers)
    if (records)
        games = list()
    for (i in 1:time)
    {
        cat(i,'\n')
        if (random)
            record = randomGames()
        else
            record = playConnectFour(W)
        if (records)
            games[[i]] = record
        W = singleTrain(record,W=W)
        if (!is.null(path) && i%%100==0)
            save(W,file=paste(path,'W.rda',sep=''))
    }
    if (records)
        return(list(W,games))
    return(W)
}

#W = trainWeight()
#W = trainWeight(W=W)
#W = trainWeight(W=W,time=1000,path='~/github/TD_ConnectFour/')

randomGames = function()
{
    cf = ConnectFour()
    record = list()
    counter = 0
    
    Ending = FALSE
    result = 0
    
    while(!Ending)
    {
        id = player(cf)
        moves = validStep(cf)
        
        if (length(moves)>1)
            mxi = sample(moves,1)
        else
            mxi = moves
            
        if (length(mxi)==0)
            browser()
        tbd = play(cf,mxi)
        
        result = win(tbd)
        if (result!=0 || full(tbd))#length(validStep(tbd))==0)
            Ending = TRUE
        
        counter = counter+1
        record[[counter]] = tbd
        cf = tbd
    }
    record
}

trainLast = function(W=NULL, layers=c(86,50,2), time=100, 
                       random=FALSE, path=NULL, records=TRUE)
{
    if (is.null(W))
        W = ParameterInitializer(layers)
    if (records)
        games = list()
    for (i in 1:time)
    {
        cat(i,'\n')
        if (random)
            record = randomGames()
        else
            record = playConnectFour(W)
        record = record[[length(record)]]
        if (records)
            games[[i]] = record
        W = singleTrain(record,W=W)
        if (!is.null(path) && i%%100==0)
            save(W,file=paste(path,'W.rda',sep=''))
    }
    if (records)
        return(list(W,games))
    return(W)
}
