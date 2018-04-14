setClass("Drop7",
         slots = c(board = "matrix",
                   num = "numeric",
                   score = "numeric",
                   update = "numeric"))

Drop7 = function(board=mat.or.vec(7,7),num=1,score=0,update=5)
{
    board=matrix(rep(Inf,49),ncol=7)
    for (i in 1:7)
    {
        n=sample(0:4,1)
        if (n>0)
            board[1:n,i]=sample(1:7,n,replace=T)
    }
    
    scoreTimer = 1
    tmp = elimFall(board,score,1)
    board = tmp[[1]]
    
    update = 5
    num = sample(1:7,1)
    new("Drop7",board=board,num=num,score=score,update=update)
}

setMethod("show",
          signature = "Drop7",
          definition = function(object)
          {
              cat('Droping',object@num,', with score',object@score,'\n')
              bd = object@board
              bd = bd[7:1,]
              cbd = as.character(bd)
              cbd[which(cbd=='Inf')] = ' '
              cbd[which(cbd=='-1')] = 'o'
              cbd[which(cbd=='-2')] = 'O'
              cbd = matrix(cbd,nrow=nrow(bd))
              cbd = data.frame(cbd)
              names(cbd) = as.character(1:7)
              show(cbd)
          }
)

setGeneric("num", function(object, ...) standardGeneric("num"))

setMethod("num",
          signature = "Drop7",
          definition = function(object, ...)
              object@num
)

setGeneric("board", function(object, ...) standardGeneric("board"))

setMethod("board",
          signature = "Drop7",
          definition = function(object, ...)
              object@board
)

setGeneric("score", function(object, ...) standardGeneric("score"))

setMethod("score",
          signature = "Drop7",
          definition = function(object, ...)
              object@score
)

setGeneric("update", function(object, ...) standardGeneric("update"))

setMethod("update",
          signature = "Drop7",
          definition = function(object, ...)
              object@update
)

setGeneric("updateDec", function(object, ...) standardGeneric("updateDec"))

setMethod("updateDec",
          signature = "Drop7",
          definition = function(object, ...)
          {
              object@update = object@update-1
              object
          }
)

setGeneric("validSteps", function(object, ...) standardGeneric("validSteps"))

setMethod("validSteps",
          signature = "Drop7",
          definition = function(object, ...)
          {
              bd = object@board
              ans = which(!is.finite(bd[7,]))
              ans
          }
)

setGeneric("feature", function(object, ...) standardGeneric("feature"))

setMethod("feature",
          signature = "Drop7",
          definition = function(object, ...)
          {
              x = c(as.vector(board(object)),player(object))
              x[which(x==-1)] = 2
              ans = mat.or.vec(2,43)
              for (i in which(x>0))
                  ans[x[i],i] = 1
              ans = as.numeric(ans)
              ans
          }
)

setGeneric("simpleFall", function(object, ...) standardGeneric("simpleFall"))

setMethod("simpleFall",
          signature = "Drop7",
          definition = function(object, tcol, fall)
          {
              board = object@board
              totalScore = object@score
              timer = object@update
              
              board = throwFall(board,tcol,fall)
              scoreTimer = 1
              tmp = elimFall(board,totalScore,scoreTimer)
              board = tmp[[1]]
              totalScore = tmp[[2]]
              
              gameEnd = nofalljudge(board)
              
              if (!gameEnd && timer==0)
              {
                  gameEnd = (gameEnd || overjudge(board))
                  if (!gameEnd)
                  {
                      board = roundAdd(board)
                      totalScore = totalScore+17000
                      gameEnd = (gameEnd || nofalljudge(board))
                      timer = 5;
                  }
              }
              tmp = elimFall(board,totalScore,scoreTimer)
              board = tmp[[1]]
              totalScore = tmp[[2]]
              
              object@board = board
              object@num = sample(1:7,1)
              object@score = totalScore
              object@update = timer
              
              gameEnd = (gameEnd || nofalljudge(board))
              
              return(list(object,gameEnd))
          }
)


#Supplimental functions

dropBoard=function(board)
{
    for (j in 1:7)
    {
        ind=which(is.finite(board[,j]))
        n=length(ind)
        if (n>0)
            board[1:n,j]=board[ind,j]
        if (n<7)
            board[(n+1):7,j]=rep(Inf,7-n)
    }
    return(board)
}

elimFall=function(board,totalScore,scoreTimer)
{
    canFall=TRUE
    #scoreTimer=1
    while (canFall)
    {
        tScore=calcScore(scoreTimer)
        canFall=FALSE
        fi=NULL
        fj=NULL
        for (i in 1:7)
            for (j in 1:7)
            {
                if (!is.na(board[i,j]) && is.finite(board[i,j]))
                {
                    scol=1
                    ss=i-1
                    while(ss>0 && is.finite(board[ss,j]))
                    {
                        ss=ss-1
                        scol=scol+1
                    }
                    ss=i+1
                    while(ss<8 && is.finite(board[ss,j]))
                    {
                        ss=ss+1
                        scol=scol+1
                    }
                    
                    srow=1
                    ss=j-1
                    while(ss>0 && is.finite(board[i,ss]))
                    {
                        ss=ss-1
                        srow=srow+1
                    }
                    ss=j+1
                    while(ss<8 && is.finite(board[i,ss]))
                    {
                        ss=ss+1
                        srow=srow+1
                    }
                    
                    if (srow==board[i,j] || scol==board[i,j])
                    {
                        fi=c(fi,i)
                        fj=c(fj,j)
                        canFall=TRUE
                    }
                }
            }
        if (canFall)
        {
            for (i in 1:length(fi))
            {
                board[fi[i],fj[i]]=Inf
                totalScore=totalScore+tScore
                ind=getind(fi[i],fj[i])
                cki=ind[[1]]
                ckj=ind[[2]]
                for (k in 1:4)
                {
                    if (board[cki[k],ckj[k]]==-2)
                        board[cki[k],ckj[k]]=-1
                    else if (board[cki[k],ckj[k]]==-1)
                        board[cki[k],ckj[k]]=sample(1:7,1)
                }
            }
            board=dropBoard(board)
            scoreTimer=scoreTimer+1
        }
    }
    return(list(board=board,totalScore=totalScore))
}

throwFall=function(board,tcol,fall)
{
    if (tcol==-1)
        gameEnd=TRUE
    else
    {
        ind=which(is.finite(board[,tcol]))
        if (length(ind)>0)
            board[ind[length(ind)]+1,tcol]=fall
        else
            board[1,tcol]=fall
    }
    return(board)
}

roundAdd=function(board)
{
    for (j in 1:7)
        board[2:7,j]=board[1:6,j]
    board[1,]=rep(-2,7)
    
    return(board)
}

nofalljudge=function(board)
{
    cfull=0
    for (j in 1:7)
    {
        if (sum(is.finite(board[,j]))==7)
            cfull=cfull+1
    }
    if (cfull==7)
        return(TRUE);
    return(FALSE)
}

overjudge=function(board)
{
    if (sum(is.finite(board[7,]))>0)
        return(TRUE);
    return(FALSE)
}

getind=function(i,j)
{
    a=c(0,1,0,-1)
    b=c(1,0,-1,0)
    ansi=a+i
    ansj=b+j
    for (i in 1:4)
    {
        ansi[i]=min(7,max(ansi[i],1))
        ansj[i]=min(7,max(ansj[i],1))
    }
    return(list(i=ansi,j=ansj))
}

calcScore=function(times)
{
    k=c(7,39,109,224,391,617,907,1267,1701,2213)
    if (times<=10)
        return(k[times])
    else
        return(12.8333-20.1226*times+
                   14.3730*times*times+
                   0.9652*times*times*times)
}


