# http://www.cis.ohio-state.edu/~jain/books/ftp/ch5e_slides.pdf
exponential.cdf =
function(x, rate = 1){
 1 - exp(-x * rate)
}

exponential.inv =
function(x, rate = 1) {
    -log(x)/rate
}


rexp =
function(n, rate = 1)
{
  u = runif(n)
  exponential.inv(u, rate)
}
