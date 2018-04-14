# Load BOLD fMRI data
setwd('~/fcfmri/')

require('SIS')
require('randomForest')

fmriacc = function (mat, lag) {
  
  p = nrow(mat)
  n = ncol(mat)
  
  if (lag > n) stop('lag must be smaller than the amino acids in the sequence')
  
  # auto covariance: p elements
  
  acc1 = matrix(0, nrow = p, ncol = lag)
  
  for (j in 1:p) {
    for (i in 1:lag) {
      acc1[j, i] = sum(mat[j, 1:(n - i)] * mat[j, ((1:(n - i)) + i)]) / (n - i)
    }
  }
  
  acc1 = as.vector(acc1)
  names(acc1) = as.vector(outer(paste0('scl', 1:p), paste0('lag', 1:lag), paste, sep = '.'))
  
  # auto cross covariance: p^2 - p elements
  
  acc2 = matrix(0, nrow = p^2 - p, ncol = lag)
  
  idx = cbind(combn(1:p, 2), combn(1:p, 2)[2:1, ])
  
  for (j in 1:ncol(idx)) {
    for (i in 1:lag) {
      acc2[j, i] = sum(mat[idx[1, j], 1:(n - i)] * mat[idx[2, j], ((1:(n - i)) + i)]) / (n - i)
    }
  }
  
  acc2 = as.vector(acc2)
  names(acc2) = as.vector(outer(paste0('scl', paste(idx[1, ], idx[2, ], sep = '.')), paste0('lag', 1:lag), paste, sep = '.'))
  
  ACC = c(acc1, acc2)
  
  return(ACC)
  
}

for ( i in 1:62 ) {
  print(i)
  mat = t(read.csv(paste0('BOLD/csv/normal_', i, '.csv'), header = FALSE))
  row.names(mat) = NULL
  eval(parse(text = paste0('acc_normal_', i, ' = fmriacc(mat, lag = 3)')))
}

for ( i in 1:69 ) {
  print(i)
  mat = t(read.csv(paste0('BOLD/csv/schizo_', i, '.csv'), header = FALSE))
  row.names(mat) = NULL
  eval(parse(text = paste0('acc_schizo_', i, ' = fmriacc(mat, lag = 3)')))
}

normalmat = matrix(NA, ncol = length(acc_normal_1), nrow = 62)
schizomat = matrix(NA, ncol = length(acc_schizo_1), nrow = 69)

for (i in 1:62) normalmat[i, ] = eval(parse(text = paste0('acc_normal_', i)))
for (i in 1:69) schizomat[i, ] = eval(parse(text = paste0('acc_schizo_', i)))

x = rbind(normalmat, schizomat)

# Naive Method

normallist = vector('list', 62)
schizolist = vector('list', 69)

for ( i in 1:62 ) {
  mat = as.vector(t(read.csv(paste0('BOLD/csv/normal_', i, '.csv'), header = FALSE)))
  normallist[[i]] = mat
}

for ( i in 1:69 ) {
  mat = as.vector(t(read.csv(paste0('BOLD/csv/schizo_', i, '.csv'), header = FALSE)))
  schizolist[[i]] = mat
}

normal_naive = do.call("rbind", normallist)
schizo_naive = do.call("rbind", schizolist)

x_naive = rbind(normal_naive, schizo_naive)

# rm(list = setdiff(ls(), c('x', 'x_naive')))

y = as.factor(c(rep(0L, 62L), rep(1L, 69L)))
ynumeric = c(rep(0L, 62L), rep(1L, 69L))



### An experiment only. See 1.cv.R for formal experiment.

vars = SIS(x, ynumeric, family = 'binomial', penalty = 'lasso', nfolds = 5)
x1 = x[, vars$ix]
y1 = as.factor(y)

names(acc_normal_1)[vars$ix]
# [1] "scl7.36.lag1"  "scl15.21.lag1" "scl34.16.lag1" "scl61.42.lag1" "scl77.57.lag1"
# [6] "scl49.38.lag3"

vars_naive = SIS(x_naive, y, family = 'binomial', penalty = 'lasso', nfolds = 5)
x1_naive = x_naive[, vars_naive$ix]

require('randomForest')
rf.fit = randomForest(x = x1, y = y1, ntree = 500L)
rf.fit
rf.fit_naive = randomForest(x = x1_naive, y = y1, ntree = 500L)
rf.fit_naive

# Fitting result
require('pROC')
roc(y, as.vector(predict(rf.fit, x1, type = 'prob')[, 2]))

require('tsne')
tsne_3d = tsne(x1, 3)
require(rgl)
plot3d(tsne_3d, col = c(rep('#e41a1c', 62), rep('#377eb8', 69)), 
       type = 's', box = FALSE, size = 1)
rgl.bringtotop()
rgl.snapshot('tsne-3d.png')
