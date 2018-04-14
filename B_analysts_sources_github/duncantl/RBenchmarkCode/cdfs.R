extreme.cdf =
function(x, a, b) {
 1 - exp(-exp((x -a)/b))
}

extreme.inv =
function(x, a, b) {
 a + b * log(log(x))
}


geometric.cdf =
function(x, p)
  1 - (1-p)^x

geometric.inv =
function(x, p)
  log(x)/(log(1-p))

logistic.cdf =
function(x, mu, b)
  1 - 1/(1 +exp((x-mu)/b))

logistic.inv =
  function(x, mu, b)
    mu - b * log(1/x - 1)

pareto.cdf =
  function(x, a)
    1 - x^(-a)

pareto.inv =
  function(x, a)
    1/(x^(1/a))

weibull.cdf =
  function(x, a, b)
    1 - exp((x/a)^b)

weibull.inv =
  function(x, a, b)
    a(log(x)^(1/b))

