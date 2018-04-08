naHeatmap <-
function(R,
                         alpha=.8,
                         main="",
                         colorkey=FALSE,
                         colorkey.orientation="bottom",
                         colorscale=c("yellow","orange","red"),
                         saving=FALSE,
                         xlab="",
                         ylab="",
                         sub="",
                         bty="o",
                         at=TRUE,
                         ...){
    n.colors  <- 10000
    my.colors <- colorRampPalette(colorscale)
    if(saving==TRUE){
       if(sum(at==TRUE)==TRUE){
          print(levelplot(transposing(R),col.regions=my.colors(n.colors), colorkey=colorkey,
              main=main,alpha.regions=alpha,pretty=TRUE,
              xlab=xlab, ylab=ylab,sub=sub,bty=bty,scales=list(draw=FALSE)))
       }else{
          print(levelplot(transposing(R),col.regions=my.colors(n.colors), colorkey=colorkey,
              main=main,alpha.regions=alpha,pretty=TRUE,
              xlab=xlab, ylab=ylab,sub=sub,bty=bty,scales=list(draw=FALSE),at=at))         
       }
    }else{
    if(sum(at==TRUE)==TRUE){
         levelplot(transposing(R),col.regions=my.colors(n.colors), colorkey=colorkey,
              main=main,alpha.regions=alpha,pretty=TRUE,
              xlab=xlab, ylab=ylab,sub=sub,bty=bty,scales=list(draw=FALSE))
       }else{
         levelplot(transposing(R),col.regions=my.colors(n.colors), colorkey=colorkey,
              main=main,alpha.regions=alpha,pretty=TRUE,
              xlab=xlab, ylab=ylab,sub=sub,bty=bty,scales=list(draw=FALSE),at=at)
       }
    }   
}# EoF

