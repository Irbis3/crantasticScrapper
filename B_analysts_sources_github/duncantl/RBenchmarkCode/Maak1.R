excitedRW2d <-
  #
  #  excitedRW2d(.5, 50000)
  #  
  # This runs in 3.1 seconds.
  # The original (in Maak.R) takes 248 seconds.
  #
  #
  # On the Intel Mac laptop, 10,000 iterations takes .75 seconds.
  # 50 replications of this  39.912
  #             system.time({x = replicate(50, excitedRW2d(.5, 10000))})
  # 100                      80.213
  # So 10,000 replications of 10,000 iterations in exciteddrift2d would take
  # at least 133 minutes. And to do this for 50 values of beta would take 
  # about 4.5 days!
  #
  # The LISP version takes ....
  #
function(beta = .05, steps = 10, probs = c(1 + beta, 1-beta, 1, 1), plot = character(), ...)
{
     # we don't really need this anymore, but it is useful to have for a complete history
  walk <- matrix(0, steps + 1, ncol = 2, dimnames = list(NULL, c("x", "y")))

  posssteps <- matrix(c(1, -1, 0, 0, 0, 0, 1, -1), ncol=2)
     # Set step 1, assuming started at 0, 0
  walk[2,] <- posssteps[sample(4, 1, prob= probs), ]

    # The trick is to have a way to quickly find out if we have been at this point
    # If we keep a count of how many times we have been to each point and put this into a
    # hash table, then we can do a fast lookup without having to loop over all the points.
  env = new.env(hash = TRUE, parent = emptyenv())

    # remember we were at 0, 0 and also wherever we are now.
  assign("0,0", 1, env)

  i = 2  
  id = paste(walk[2,], collapse = ",")
  assign(id, 1, env)
  
  while( i <= steps) {
    ctr = 0   # the number of times we have been at the current site.

       #(exists(id, env, inherits = FALSE) &&
    ctr = get(id, env, inherits = FALSE)
    eaten = ctr > 1

        # sample the row index of possteps with probabilities depending
        # on eaten.
        # Was
        #    sample(4, 1, prob = if(eaten)  c(1, 1, 1, 1) else probs)
        # but sample() is slower.
    #print(c(id, ctr, eaten))
    chosenstep <- if(eaten) floor(runif(1, 1, 5)) else sample(4, 1, prob = probs)
    walk[i+1, ] <- walk[i,] + posssteps[chosenstep,]

       # update how often we have been at this site.
      #  id = paste(walk[i+1,], collapse = ",")
      # Using sprintf rather than paste() speeds things up a good bit,
      # almost a factor of 2.
    id = sprintf("%d,%d", walk[i+1, 1], walk[i+1, 2])
    ctr = if(exists(id, env, inherits = FALSE))
                get(id, env, inherits = FALSE)
           else
                0
    assign(id, ctr + 1, env)
    
    i = i + 1
  }


     # if plot is specified, it can be FALSE, TRUE or a string giving the prefix
     # for the file names to be created.
  if(length(plot) && (is.character(plot)  || (is.logical(plot) && plot))) {
    
    region <- max(-min(walk), max(walk))
    Vapprox <- walk[steps+1,1]/steps
    for(k in 1:steps) {
      if(is.character(plot))
          jpeg(file = paste(plot, k, ".jpg", sep = ""), bg = "transparent", ...)
      plot(c(-10 + Vapprox*k, 10 + Vapprox*k), c(-10, 10), xlab="",ylab="",type="n")
      abline(v = -(region+10):(region+10),col=3)
      abline(h = -(region+10):(region+10),col=3)
      xcoords <- rep(-(region+10):(region+10),2*(region+10)+1)
      ycoords <- rep(-(region+10):(region+10),rep(2*(region+10)+1,2*(region+10)+1))
      points(xcoords, ycoords, pch=19, lwd=1, col = "red")
      points(walk[1:k,1], walk[1:k,2], pch=19, col=3, lwd=1)
      points(walk[k,1], walk[k,2], pch=19, col=1, lwd=5)
      if(is.character(plot))
        dev.off()
    }
   }

   # This gets a vector of how often each site has been visited
   #  tb = sapply(objects(env), get, env)
   #  print(list(walk, tb))

#  list(x_1 = walk[steps+1, 1])
  walk
}

####################################



exciteddrift2d <- function(betavec,steps,reps, plot = TRUE){
  vbeta <- rep(NA,length(betavec))
  sdbeta <- rep(NA,length(betavec))
  for(i in 1:length(betavec)){
    data <- rep(NA,reps)
    for(j in 1:reps)
      data[j] <- excitedRW2d(betavec[i],steps)$x_1

    vbeta[i] <- mean(data)/steps
    sdbeta[i] <- sd(data)/sqrt(steps)}
  if(plot) {
	  plot(c(betavec,betavec),c(vbeta,sdbeta),type="n")
	  lines(betavec,vbeta,col=1)
	  lines(betavec,sdbeta,col=2)
  }
}
