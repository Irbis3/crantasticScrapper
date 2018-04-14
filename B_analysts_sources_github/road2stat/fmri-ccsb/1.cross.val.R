# Cross validation
setwd('~/fcfmri/')

require('randomForest')
require('glmnet')
require('SIS')
require('foreach')
require('doMC')

registerDoMC(4)

largecv = function (y, x, nfolds = 5L, reptimes = 10L, 
                    ntree = 500L, rnd.seed = 42L) {
  
  set.seed(rnd.seed)
  
  for ( i in 1L:reptimes ) eval(parse(text = paste0('rep',  i, 'fold = as.factor(sample(rep_len(1:nfolds, nrow(x))))')))
  
  cvres = foreach( j = 1L:reptimes ) %:%
    
    foreach( i = 1L:nfolds ) %dopar% {
      
      foldlvl = eval(parse(text = paste0('levels(rep', j, 'fold)')))
      idx = eval(parse(text = paste0('which(rep', j, 'fold == foldlvl[i])')))
      
      # split training / test set
      xtr = x[-idx, ]
      xte = x[idx, ]
      ytr = y[-idx]
      yte = y[idx]
      
      # SIS
      vars = SIS(xtr, as.numeric(as.character(ytr)), 
                 family = 'binomial', penalty = 'lasso', tune = 'cv', nfolds = 5)
      xtr = xtr[, vars$ix]
      xte = xte[, vars$ix]
      
      fit = randomForest(xtr, ytr, ntree = ntree)
      
      pred.resp = as.vector(predict(fit, xte, type = 'response'))
      pred.prob = as.vector(predict(fit, xte, type = 'prob')[, 1])
      real.resp = as.vector(yte)
      
      data.frame(idx, real.resp, pred.resp, pred.prob)
      
    }
  
  return(cvres)
  
}

# 5-fold repeated 10 times CV
eval(parse(text = paste0('x_cvres = largecv(y, x)')))
save(x_cvres, file = 'x_cvres.rda')
eval(parse(text = paste0('x_naive_cvres = largecv(y, x_naive)')))
save(x_naive_cvres, file = 'x_naive_cvres.rda')

# evaluation

require('pROC')

load('x_cvres.rda')
load('x_naive_cvres.rda')

targetid = c('x_cvres', 'x_naive_cvres')

# pre-process the model data: combine inner 5-fold
for ( j in targetid ){
  for ( i in 1L:10L ) {
    eval(parse(text = paste0(j, '[[', i, ']] = do.call(rbind, ', j, '[[', i, ']])')))
  }
}

aucmat = matrix(NA, nrow = length(targetid), ncol = 10L)

for ( i in 1L:length(targetid) ) {
  for ( j in 1L:10L ) {
    tmp = eval(parse(text = paste0(targetid[i], '[[', j, ']]')))
    aucmat[i, j] = as.numeric(auc(tmp$real.resp, tmp$pred.prob))
  }
}

row.names(aucmat) = targetid

tpmat = matrix(NA, nrow = length(targetid), ncol = 10L)

for ( i in 1L:length(targetid) ) {
  for ( j in 1L:10L ) {
    tmp = eval(parse(text = paste0(targetid[i], '[[', j, ']]')))
    tpmat[i, j] = sum(tmp[which(tmp$real.resp == 1L), 'pred.resp'] == 1L)
  }
}

row.names(tpmat) = targetid

tnmat = matrix(NA, nrow = length(targetid), ncol = 10L)

for ( i in 1L:length(targetid) ) {
  for ( j in 1L:10L ) {
    tmp = eval(parse(text = paste0(targetid[i], '[[', j, ']]')))
    tnmat[i, j] = sum(tmp[which(tmp$real.resp == 0L), 'pred.resp'] == 0L)
  }
}

row.names(tnmat) = targetid

fpmat = matrix(NA, nrow = length(targetid), ncol = 10L)

for ( i in 1L:length(targetid) ) {
  for ( j in 1L:10L ) {
    tmp = eval(parse(text = paste0(targetid[i], '[[', j, ']]')))
    fpmat[i, j] = sum(tmp[which(tmp$real.resp == 0L), 'pred.resp'] == 1L)
  }
}

row.names(fpmat) = targetid

fnmat = matrix(NA, nrow = length(targetid), ncol = 10L)

for ( i in 1L:length(targetid) ) {
  for ( j in 1L:10L ) {
    tmp = eval(parse(text = paste0(targetid[i], '[[', j, ']]')))
    fnmat[i, j] = sum(tmp[which(tmp$real.resp == 1L), 'pred.resp'] == 0L)
  }
}

row.names(fnmat) = targetid

pmat = matrix(NA, nrow = length(targetid), ncol = 10L)

for ( i in 1L:length(targetid) ) {
  for ( j in 1L:10L ) {
    tmp = eval(parse(text = paste0(targetid[i], '[[', j, ']]')))
    pmat[i, j] = sum(tmp$real.resp == 1L)
  }
}

row.names(pmat) = targetid

nmat = matrix(NA, nrow = length(targetid), ncol = 10L)

for ( i in 1L:length(targetid) ) {
  for ( j in 1L:10L ) {
    tmp = eval(parse(text = paste0(targetid[i], '[[', j, ']]')))
    nmat[i, j] = sum(tmp$real.resp == 0L)
  }
}

row.names(nmat) = targetid

accuracymat = (tpmat + tnmat) / (pmat + nmat)
errorratemat = (fpmat + fnmat) / (pmat + nmat)
sensitivitymat = tpmat / pmat
specificitymat = tnmat / nmat
precisionmat = tpmat / (tpmat + fpmat)
fscoremat = (2 * precisionmat * sensitivitymat) / (precisionmat + sensitivitymat)



finalmat = matrix(NA, nrow = length(targetid), ncol = 11 * 2)  # mean column + sd column

colnames(finalmat) = c(
  'AUC', 'AUC.SD', 
  'Accuracy', 'Accuracy.SD', 
  'Precision', 'Precision.SD', 
  'TP', 'TP.SD', 
  'TN', 'TN.SD', 
  'FP', 'FP.SD', 
  'FN', 'FN.SD', 
  'Sensitivity', 'Sensitivity.SD', 
  'Specificity', 'Specificity.SD', 
  'ErrorRate', 'ErrorRate.SD', 
  'FScore', 'FScore.SD')

finalmat[, 'AUC'] = rowMeans(aucmat)
finalmat[, 'AUC.SD'] = apply(aucmat, 1L, sd)
finalmat[, 'Accuracy'] = rowMeans(accuracymat)
finalmat[, 'Accuracy.SD'] = apply(accuracymat, 1L, sd)
finalmat[, 'Precision'] = rowMeans(precisionmat)
finalmat[, 'Precision.SD'] = apply(precisionmat, 1L, sd)
finalmat[, 'TP'] = rowMeans(tpmat)
finalmat[, 'TP.SD'] = apply(tpmat, 1L, sd)
finalmat[, 'TN'] = rowMeans(tnmat)
finalmat[, 'TN.SD'] = apply(tnmat, 1L, sd)
finalmat[, 'FP'] = rowMeans(fpmat)
finalmat[, 'FP.SD'] = apply(fpmat, 1L, sd)
finalmat[, 'FN'] = rowMeans(fnmat)
finalmat[, 'FN.SD'] = apply(fnmat, 1L, sd)
finalmat[, 'Sensitivity'] = rowMeans(sensitivitymat)
finalmat[, 'Sensitivity.SD'] = apply(sensitivitymat, 1L, sd)
finalmat[, 'Specificity'] = rowMeans(specificitymat)
finalmat[, 'Specificity.SD'] = apply(specificitymat, 1L, sd)
finalmat[, 'ErrorRate'] = rowMeans(errorratemat)
finalmat[, 'ErrorRate.SD'] = apply(errorratemat, 1L, sd)
finalmat[, 'FScore'] = rowMeans(fscoremat)
finalmat[, 'FScore.SD'] = apply(fscoremat, 1L, sd)

require('RColorBrewer')

redpal = brewer.pal(9, 'OrRd')
grnpal = brewer.pal(9, 'BuGn')

pdf('fmri-roc-smooth.pdf')

plot(smooth(roc(x_cvres[[1]]$real.resp, x_cvres[[1]]$pred.prob)), col = redpal[1])
plot(smooth(roc(x_cvres[[2]]$real.resp, x_cvres[[2]]$pred.prob)), col = redpal[2], add = TRUE)
plot(smooth(roc(x_cvres[[3]]$real.resp, x_cvres[[3]]$pred.prob)), col = redpal[3], add = TRUE)
plot(smooth(roc(x_cvres[[4]]$real.resp, x_cvres[[4]]$pred.prob)), col = redpal[4], add = TRUE)
plot(smooth(roc(x_cvres[[5]]$real.resp, x_cvres[[5]]$pred.prob)), col = redpal[5], add = TRUE)
plot(smooth(roc(x_cvres[[6]]$real.resp, x_cvres[[6]]$pred.prob)), col = redpal[6], add = TRUE)
plot(smooth(roc(x_cvres[[7]]$real.resp, x_cvres[[7]]$pred.prob)), col = redpal[7], add = TRUE)
plot(smooth(roc(x_cvres[[8]]$real.resp, x_cvres[[8]]$pred.prob)), col = redpal[8], add = TRUE)
plot(smooth(roc(x_cvres[[9]]$real.resp, x_cvres[[9]]$pred.prob)), col = redpal[9], add = TRUE)
plot(smooth(roc(x_cvres[[10]]$real.resp, x_cvres[[10]]$pred.prob)), col = redpal[9], add = TRUE)

plot(smooth(roc(x_naive_cvres[[1]]$real.resp, x_naive_cvres[[1]]$pred.prob)), col = grnpal[1], add = TRUE)
plot(smooth(roc(x_naive_cvres[[2]]$real.resp, x_naive_cvres[[2]]$pred.prob)), col = grnpal[2], add = TRUE)
plot(smooth(roc(x_naive_cvres[[3]]$real.resp, x_naive_cvres[[3]]$pred.prob)), col = grnpal[3], add = TRUE)
plot(smooth(roc(x_naive_cvres[[4]]$real.resp, x_naive_cvres[[4]]$pred.prob)), col = grnpal[4], add = TRUE)
plot(smooth(roc(x_naive_cvres[[5]]$real.resp, x_naive_cvres[[5]]$pred.prob)), col = grnpal[5], add = TRUE)
plot(smooth(roc(x_naive_cvres[[6]]$real.resp, x_naive_cvres[[6]]$pred.prob)), col = grnpal[6], add = TRUE)
plot(smooth(roc(x_naive_cvres[[7]]$real.resp, x_naive_cvres[[7]]$pred.prob)), col = grnpal[7], add = TRUE)
plot(smooth(roc(x_naive_cvres[[8]]$real.resp, x_naive_cvres[[8]]$pred.prob)), col = grnpal[8], add = TRUE)
plot(smooth(roc(x_naive_cvres[[9]]$real.resp, x_naive_cvres[[9]]$pred.prob)), col = grnpal[9], add = TRUE)
plot(smooth(roc(x_naive_cvres[[10]]$real.resp, x_naive_cvres[[10]]$pred.prob)), col = grnpal[9], add = TRUE)

legend('bottomright', legend = c('ACC Method (0.75)', 'Naive Method (0.54)'),
       col = c(redpal[8], grnpal[8]), 
       lwd = 2, seg.len = 4, 
       title = 'Methods', 
       cex = 1.2)

dev.off()

pdf('fmri-roc-nosmooth.pdf')

plot(roc(x_cvres[[1]]$real.resp, x_cvres[[1]]$pred.prob), col = redpal[1])
plot(roc(x_cvres[[2]]$real.resp, x_cvres[[2]]$pred.prob), col = redpal[2], add = TRUE)
plot(roc(x_cvres[[3]]$real.resp, x_cvres[[3]]$pred.prob), col = redpal[3], add = TRUE)
plot(roc(x_cvres[[4]]$real.resp, x_cvres[[4]]$pred.prob), col = redpal[4], add = TRUE)
plot(roc(x_cvres[[5]]$real.resp, x_cvres[[5]]$pred.prob), col = redpal[5], add = TRUE)
plot(roc(x_cvres[[6]]$real.resp, x_cvres[[6]]$pred.prob), col = redpal[6], add = TRUE)
plot(roc(x_cvres[[7]]$real.resp, x_cvres[[7]]$pred.prob), col = redpal[7], add = TRUE)
plot(roc(x_cvres[[8]]$real.resp, x_cvres[[8]]$pred.prob), col = redpal[8], add = TRUE)
plot(roc(x_cvres[[9]]$real.resp, x_cvres[[9]]$pred.prob), col = redpal[9], add = TRUE)
plot(roc(x_cvres[[10]]$real.resp, x_cvres[[10]]$pred.prob), col = redpal[9], add = TRUE)

plot(roc(x_naive_cvres[[1]]$real.resp, x_naive_cvres[[1]]$pred.prob), col = grnpal[1], add = TRUE)
plot(roc(x_naive_cvres[[2]]$real.resp, x_naive_cvres[[2]]$pred.prob), col = grnpal[2], add = TRUE)
plot(roc(x_naive_cvres[[3]]$real.resp, x_naive_cvres[[3]]$pred.prob), col = grnpal[3], add = TRUE)
plot(roc(x_naive_cvres[[4]]$real.resp, x_naive_cvres[[4]]$pred.prob), col = grnpal[4], add = TRUE)
plot(roc(x_naive_cvres[[5]]$real.resp, x_naive_cvres[[5]]$pred.prob), col = grnpal[5], add = TRUE)
plot(roc(x_naive_cvres[[6]]$real.resp, x_naive_cvres[[6]]$pred.prob), col = grnpal[6], add = TRUE)
plot(roc(x_naive_cvres[[7]]$real.resp, x_naive_cvres[[7]]$pred.prob), col = grnpal[7], add = TRUE)
plot(roc(x_naive_cvres[[8]]$real.resp, x_naive_cvres[[8]]$pred.prob), col = grnpal[8], add = TRUE)
plot(roc(x_naive_cvres[[9]]$real.resp, x_naive_cvres[[9]]$pred.prob), col = grnpal[9], add = TRUE)
plot(roc(x_naive_cvres[[10]]$real.resp, x_naive_cvres[[10]]$pred.prob), col = grnpal[9], add = TRUE)

legend('bottomright', legend = c('ACC Method (0.75)', 'Naive Method (0.54)'),
       col = c(redpal[8], grnpal[8]), 
       lwd = 2, seg.len = 4, 
       title = 'Methods', 
       cex = 1.2)

dev.off()
