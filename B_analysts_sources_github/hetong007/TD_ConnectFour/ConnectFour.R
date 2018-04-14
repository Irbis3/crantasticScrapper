setClass("ConnectFour",
         slots = c(board = "matrix",
                   player = "numeric"))

ConnectFour = function(board=mat.or.vec(6,7),player=1)
{
    new("ConnectFour",board=board,player=player)
}

setMethod("show",
          signature = "ConnectFour",
          definition = function(object)
          {
              cat('Waiting for player',object@player,'\n')
              bd = object@board
              cbd = as.character(bd)
              cbd[which(cbd=='0')] = ' '
              cbd[which(cbd=='1')] = 'o'
              cbd[which(cbd=='-1')] = 'x'
              cbd = matrix(cbd,nrow=nrow(bd))
              cbd = data.frame(cbd)
              names(cbd) = as.character(1:7)
              show(cbd)
          }
)

setGeneric("player", function(object, ...) standardGeneric("player"))

setMethod("player",
          signature = "ConnectFour",
          definition = function(object, ...)
              object@player
)

setGeneric("board", function(object, ...) standardGeneric("board"))

setMethod("board",
          signature = "ConnectFour",
          definition = function(object, ...)
              object@board
)

setGeneric("feature", function(object, ...) standardGeneric("feature"))

setMethod("feature",
          signature = "ConnectFour",
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

setGeneric("full", function(object, ...) standardGeneric("full"))

setMethod("full",
          signature = "ConnectFour",
          definition = function(object, ...)
          {
              bd = object@board
              ans = all(bd!=0)
              ans
          }
)

setGeneric("validStep", function(object, ...) standardGeneric("validStep"))

setMethod("validStep",
          signature = "ConnectFour",
          definition = function(object, ...)
          {
              bd = object@board
              ans = which(bd[1,]==0)
              ans
          }
)

setGeneric("changePlayer", function(object, ...) standardGeneric("changePlayer"))

setMethod("changePlayer",
          signature = "ConnectFour",
          definition = function(object, ...)
          {
              object@board = -object@board
              object
          }
)

setGeneric("play", function(object, ...) standardGeneric("play"))

setMethod("play",
          signature = "ConnectFour",
          definition = function(object, i, ...)
          {
              bd = object@board
              plyr = object@player
              if (i<1 || i>ncol(bd))
                  stop('Invalid move.')
              if (bd[1,i]!=0)
                  stop('Full!')
              ind = which(bd[,i]!=0)[1]-1
              if (is.na(ind))
                  ind = nrow(bd)
              bd[ind,i] = plyr
              ans = ConnectFour(bd,-plyr)
              ans
          }
)

setGeneric("win", function(object, ...) standardGeneric("win"))

setMethod("win",
          signature = "ConnectFour",
          definition = function(object, ...)
          {
              bd = object@board
              n = nrow(bd)
              m = ncol(bd)
              
              for (i in 1:(n-3))
                  for (j in 1:m)
                  {
                      tmp = bd[i,j]
                      if (tmp!=0)
                      {
                          cnt = 1
                          while (cnt<4 && bd[i+cnt,j]==tmp)
                              cnt = cnt+1
                          if (cnt>=4)
                              return(tmp)
                      }
                  }
              
              for (i in 1:n)
                  for (j in 1:(m-3))
                  {
                      tmp = bd[i,j]
                      if (tmp!=0)
                      {
                          cnt = 1
                          while (cnt<4 && bd[i,j+cnt]==tmp)
                              cnt = cnt+1
                          if (cnt>=4)
                              return(tmp)
                      }
                  }
              
              for (i in 1:(n-3))
                  for (j in 1:(m-3))
                  {
                      tmp = bd[i,j]
                      if (tmp!=0)
                      {
                          cnt = 1
                          while (cnt<4 && bd[i+cnt,j+cnt]==tmp)
                              cnt = cnt+1
                          if (cnt>=4)
                              return(tmp)
                      }
                  }
              
              for (i in 4:n)
                  for (j in 1:(m-3))
                  {
                      tmp = bd[i,j]
                      if (tmp!=0)
                      {
                          cnt = 1
                          while (cnt<4 && bd[i-cnt,j+cnt]==tmp)
                              cnt = cnt+1
                          if (cnt>=4)
                              return(tmp)
                      }
                  }
              
              return(0)
          }
)
