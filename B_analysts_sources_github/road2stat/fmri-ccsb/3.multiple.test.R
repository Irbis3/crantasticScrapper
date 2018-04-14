# Multiple comparison test with FDR control
require('AnalyzeFMRI')
require('fdrtool')

x_acc_only = x[, -c(1:270)]
colnames(x_acc_only) = names(acc_normal_1)[-c(1:270)]

df = matrix(NA, ncol(x_acc_only), 2L)
df1 = matrix(NA, ncol(x_acc_only)/3, 2L)
df2 = matrix(NA, ncol(x_acc_only)/3, 2L)
df3 = matrix(NA, ncol(x_acc_only)/3, 2L)

for ( i in 1:ncol(x_acc_only) ) {
  print(i)
  tmp = t.test(x_acc_only[1:62, i], x_acc_only[63:131, i])
  df[i, ] = c(tmp$statistic, tmp$p.value)
}

colnames(df) = c('statistic', 'pvalue')

df1 = df[1:8010, ]
df2 = df[8011:16020, ]
df3 = df[16021:24030, ]

pdf('fdr.lags.pdf')
plot(density(df1[, 'pvalue']), main = 'Density estimation of p-values', lwd = 3, col = 'darkgreen')
lines(density(df2[, 'pvalue']), lwd = 3, col = 'blue')
lines(density(df3[, 'pvalue']), lwd = 3, col = 'red')
dev.off()

thr1 = Threshold.FDR(df1[, 1L], q = 0.1, cV.type = 2, type = 't', df1 = 131 - 1)
thr2 = Threshold.FDR(df2[, 1L], q = 0.1, cV.type = 2, type = 't', df1 = 131 - 1)
thr3 = Threshold.FDR(df3[, 1L], q = 0.1, cV.type = 2, type = 't', df1 = 131 - 1)

colnames(x_acc_only)[1:8010][which(df1[, 'statistic'] >= thr1)]
colnames(x_acc_only)[8011:16020][which(df2[, 'statistic'] >= thr2)]
colnames(x_acc_only)[16021:24030][which(df3[, 'statistic'] >= thr3)]

df1[which(df1[, 'statistic'] >= thr1), ]
df2[which(df2[, 'statistic'] >= thr2), ]
df3[which(df3[, 'statistic'] >= thr3), ]
