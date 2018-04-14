
readData =
  #
  # Write a function to read the data and convert the 
  # date column to a POSIX
  #
function(fileName, dateFormat = c("%Y-%m-%d", "%Y/%m/%d"))
{
   data = read.csv(fileName, header = TRUE, stringsAsFactors = FALSE)
   for(fmt in dateFormat) {
      tmp = as.Date(data$Date, fmt)
      if(all(!is.na(tmp))) {
         data$Date = tmp
         break
      }
   }

   data
 }





combine2Stocks = 
function(a, b, stockNames = c(deparse(substitute(a)), deparse(substitute(b))))
{
  rr = range(intersect(a$Date, b$Date))
  a.sub = a[ a$Date >= rr[1] & a$Date <= rr[2],]
  b.sub = b[ b$Date >= rr[1] & b$Date <= rr[2],]
  structure(data.frame(a.sub$Date, a.sub$Adj.Close, b.sub$Adj.Close),
             names = c("Date", stockNames))
}





plotRatio =
function(r, k = 1, date = seq(along = r), ...)
{
  plot(date, r, type = "l", ...)
  abline(h = c(mean(r), mean(r) + k * sd(r), mean(r) - k * sd(r)), 
         col = c("green", rep("red", 2*length(k))), lty = "dashed")
}





findNextPosition =
  #
  #  findNextPosition(r)
  #  findNextPosition(r, 1174)
  # Check they are increasing and correctly offset
  #
  
function(ratio, startDay = 1, k = 1, m = mean(ratio), s = sd(ratio))
{
  up = m + k *s
  down = m - k *s

  if(startDay > 1)
     ratio = ratio[ - (1:(startDay-1)) ]
    
  isExtreme = ratio >= up | ratio <= down
  
  w = which(isExtreme)
  if(length(w) == 0)
      return(integer())

  start = w[1]
  backToNormal = if(ratio[start] > up)
                     ratio[ - (1:start) ] <= m
                 else
                     ratio[ - (1:start) ] >= m

   # return either the end of the position or the end of the
   # Could return NA for not ended, i.e. which(backToNormal)[1]
   # for both cases. But then the caller has to interpret that.
   
  end = if(any(backToNormal))
          which(backToNormal)[1] + start
        else
          length(ratio)
  
  c(start, end) + startDay - 1 # check we are correct and not off by 1.
}





showPosition = 
function(days, ratio, radius = 30)
{
  symbols(days, ratio[days], circles = rep(radius, 2), 
           fg = c("green", "red"), add = TRUE, inches = FALSE)
}





getPositions =
function(ratio, k = 1, m = mean(ratio), s = sd(ratio))
{
   when = list()
   cur = 1

   while(cur < length(ratio)) {
      tmp = findNextPosition(ratio, cur, k, m, s)
      if(length(tmp) == 0)  # done
         break
      when[[length(when) + 1]] = tmp
      if(is.na(tmp[2]) || tmp[2] == length(ratio))
         break
      cur = tmp[2] + 1
    }

   when
}





showPosition = 
function(days, ratio, radius = 30)
{
  if(is.list(days))
     days = unlist(days)

  symbols(days, ratio[days], circles = rep(radius, length(days)), 
          fg = c("green", "red"),
          add = TRUE, inches = FALSE)
}





positionProfit =
  #
  #  r = overlap$att/overlap$verizon
  #  k = 1.7
  #  pos = getPositions(r, k)
  #  positionProfit(pos[[1]], overlap$att, overlap$verizon)
  #
function(pos, x1, x2, ratioMean = mean(x1/x2), p = .001, byStock = FALSE)
{
  if(is.list(pos)) {
    ans = sapply(pos, positionProfit, x1, x2, ratioMean, p, byStock)
    if(byStock)
       rownames(ans) = c("A", "B", "commission")
    return(ans)
  }
  
  priceA = x1[pos]
  priceB = x2[pos]

  unitsOfA = 1/priceA[1]
  unitsOfB = 1/priceB[1]

  amt = c(unitsOfA * priceA[2], priceB[2] * unitsOfB)

     # Which are we selling
  sellWhat = if(priceA[1]/priceB[1] > ratioMean) "A" else "B"

  profit = if(sellWhat == "A") {
              c((1 - amt[1]),  (amt[2] - 1), - p * sum(amt))
           } else {
              c( (1 - amt[2]),  (amt[1] - 1),  - p * sum(amt))
           }

  if(byStock)
     profit
  else
     sum(profit)
}





stockSim <- 
function(n = 4000, rho = 0.99, psi = 0, sigma = rep(1, 2),
         beta0 = rep(100, 2), beta1 = rep(0, 2),
         epsilon = matrix(rnorm(2*n, sd = sigma), nrow = n))
{
  X <- matrix(NA, nrow = n, ncol = 2)
  X[1,] <- epsilon[1,]

  A <- matrix(c(rho, psi*(1-rho), psi*(1-rho), rho), nrow = 2)
  for(i in 2:n){
     # X[i, 1] = rho * X[i-1, 1] + psi * ( 1 - rho ) * X[i - 1, 2] + epsilon[i, 1]
     # X[i, 2] = rho * X[i-1, 2] + psi * ( 1 - rho ) * X[i - 1, 1] + epsilon[i, 2]
    X[i,] <- A %*% X[i-1,] + epsilon[i,]
  }
  
   ## Add in the trends
  X[,1] <- beta0[1] + beta1[1] * (1:n) + X[,1]
  X[,2] <- beta0[2] + beta1[2] * (1:n) + X[,2]

  X
}





rescale = 
function(x, y, lower = min(x, na.rm = TRUE), upper = max(x, na.rm = TRUE))
{
  slope <- (upper - lower) / diff(range(x, na.rm = TRUE))
  intercept <- upper - slope * max(x, na.rm = TRUE)
  y <- intercept + slope * x
  return(list(new = y, coef = c(intercept, slope)))
}





twoplot <- function(x, y1, y2, type = "l", main = NULL, add.col = "dark gray",
                    xlab = paste(deparse(substitute(x))),
                    ylab1 = paste(deparse(substitute(y1))),
                    ylab2 = paste(deparse(substitute(y2))),
                    mar = c(5, 4, 4, 5), ...){

  # Check that x, y1, and y2 all have the same length
  stopifnot(length(x) == length(y1), length(x) == length(y2),
            length(y1) == length(y2))

       # Make room for the extra right-hand axis
  par(mar = mar)

       # Plot y1 against x
  plot(x, y1, type = type, xlab = xlab, ylab = ylab1, main = main, ...)

       # Rescale y2 to have the same min and max as y1; save the coefficients
  setup <- rescale(y2, lower = min(y1, na.rm = TRUE),
                   upper = max(y1, na.rm = TRUE))
  y2.rescale <- setup$new

    # Plot the rescaled y2 against x
  points(x, y2.rescale, type = type, col = add.col)

    # Compute nice labels for y2
  y2.labels <- pretty(y2, n = 5)
    # Rescale the labels
  y2.labels.rescale <- setup$coef[1] + setup$coef[2] * y2.labels
    # Add the axis to the right hand side
  axis(side = 4, at = y2.labels.rescale,
       labels = y2.labels, col.axis = add.col, col = add.col)
     # Add the extra axis label
  mtext(text = ylab2, side = 4, line = 3, col = add.col)

  NULL
}





getBestK = 
function(x, y, ks = seq(0.1, max.k, length = N), N = 100, 
         max.k = NA, m = mean(x/y))
{
    if(is.na(max.k)) {
       r = x/y
       max.k = max(r/sd(r))
    }

    pr.k = sapply(ks, getProfit.K, x, y, m = m)
    median(ks[ pr.k == max(pr.k) ])
}





getProfit.K =
function(k, x, y, m = mean(x/y)) 
{
    pos = getPositions(x/y, k, m = m)
    if(length(pos) == 0)  
       0
    else
       sum(positionProfit(pos, x, y, m))
}





runSim = 
function(rho, psi, beta = c(0, 0), B = 999, sigma = c(1, 1))
{
   pr = replicate(B,  {
                        X = stockSim(4000, rho, psi, sigma, beta1 = beta)
                        train = X[1:2000, ]
                        test = X[2001:4000, ]
                        m = mean(train[,1]/train[,2])
                        k.star = getBestK(train[1,], train[2,], m = m)
                        getProfit.K(k.star, test[1,], test[2,], m)
                      })
}

