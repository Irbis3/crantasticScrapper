# Networks derived by correlation matrix
require('qgraph')
require('fdrtool')
require('huge')

x_acc_only = x[, -c(1:270)]
colnames(x_acc_only) = names(acc_normal_1)[-c(1:270)]
x_acc_only_normal = abs(apply(x_acc_only[1:62, ], 2, mean))
x_acc_only_schizo = abs(apply(x_acc_only[63:131, ], 2, mean))

xcor_schizo_lag1 = diag(90L)
xcor_normal_lag1 = diag(90L)
xcor_schizo_lag2 = diag(90L)
xcor_normal_lag2 = diag(90L)
xcor_schizo_lag3 = diag(90L)
xcor_normal_lag3 = diag(90L)

for ( i in 1:89 ) {
  xcor_schizo_lag1[i, (i + 1):90L] = x_acc_only_schizo[grep(paste0('scl', i, '\\..*\\.lag1'), colnames(x_acc_only))[1L:(90 - i)]]
}

for ( i in 1:89 ) {
  xcor_schizo_lag2[i, (i + 1):90L] = x_acc_only_schizo[grep(paste0('scl', i, '\\..*\\.lag2'), colnames(x_acc_only))[1L:(90 - i)]]
}

for ( i in 1:89 ) {
  xcor_schizo_lag3[i, (i + 1):90L] = x_acc_only_schizo[grep(paste0('scl', i, '\\..*\\.lag3'), colnames(x_acc_only))[1L:(90 - i)]]
}

for ( i in 1:89 ) {
  xcor_normal_lag1[i, (i + 1):90L] = x_acc_only_normal[grep(paste0('scl', i, '\\..*\\.lag1'), colnames(x_acc_only))[1L:(90 - i)]]
}

for ( i in 1:89 ) {
  xcor_normal_lag2[i, (i + 1):90L] = x_acc_only_normal[grep(paste0('scl', i, '\\..*\\.lag2'), colnames(x_acc_only))[1L:(90 - i)]]
}

for ( i in 1:89 ) {
  xcor_normal_lag3[i, (i + 1):90L] = x_acc_only_normal[grep(paste0('scl', i, '\\..*\\.lag3'), colnames(x_acc_only))[1L:(90 - i)]]
}

xcor_schizo_lag1[lower.tri(xcor_schizo_lag1)] = t(xcor_schizo_lag1)[lower.tri(t(xcor_schizo_lag1))]
xcor_schizo_lag2[lower.tri(xcor_schizo_lag2)] = t(xcor_schizo_lag2)[lower.tri(t(xcor_schizo_lag2))]
xcor_schizo_lag3[lower.tri(xcor_schizo_lag3)] = t(xcor_schizo_lag3)[lower.tri(t(xcor_schizo_lag3))]

xcor_normal_lag1[lower.tri(xcor_normal_lag1)] = t(xcor_normal_lag1)[lower.tri(t(xcor_normal_lag1))]
xcor_normal_lag2[lower.tri(xcor_normal_lag2)] = t(xcor_normal_lag2)[lower.tri(t(xcor_normal_lag2))]
xcor_normal_lag3[lower.tri(xcor_normal_lag3)] = t(xcor_normal_lag3)[lower.tri(t(xcor_normal_lag3))]

### Network analysis ###

require("psych")
data(bfi)

data(big5)

groups = rep(c(5, 1, 2, 2, 2, 2, 2, 2, 4, 5, 6, 1, 1, 1, 4, 
               1, 6, 1, 6, 6, 6, 3, 3, 3, 3, 3, 3, 3, 5, 2, 
               2, 4, 1, 1, 5, 6, 6, 6, 6, 4, 4, 4, 1, 1, 6), each = 2)

grps = vector('list', 6)
for ( i in 1:90 ) grps[[groups[i]]] = append(grps[[groups[i]]], i)
names(grps) = paste('ROI Group', 1:6)

z = qgraph(xcor_schizo_lag1, vsize = 2, graph = "sig", alpha = c(1e-5, 1e-4, 1e-3), 
           groups = grps,
           details = TRUE)
z = qgraph(xcor_schizo_lag2, vsize = 2, graph = "sig", alpha = c(1e-5, 1e-4, 1e-3), 
           details = TRUE)

png('net-schizo.png', width = 1400, height = 1024)
z = qgraph(xcor_schizo_lag3, vsize = 2, 
           graph = "sig", alpha = c(1e-8, 1e-6, 1e-3), 
           groups = grps, 
           edge.color = brewer.pal(9, 'Reds')[6], 
           details = FALSE, edge.width = 0.2, 
           borders = FALSE, title = "")
dev.off()

png('net-blank.png', width = 1400, height = 1024)
z = qgraph(xcor_schizo_lag3, vsize = 2, 
           graph = "sig", alpha = c(1e-8, 1e-6, 1e-3), 
           groups = grps, 
           edge.color = 'lightgrey', 
           details = FALSE, edge.width = 0.2, 
           borders = FALSE, title = "")
dev.off()

z = qgraph(xcor_normal_lag1, vsize = 2, graph = "sig", alpha = c(1e-5, 1e-4, 1e-3), 
           details = TRUE)
z = qgraph(xcor_normal_lag2, vsize = 2, graph = "sig", alpha = c(1e-5, 1e-4, 1e-3), 
           details = TRUE)

png('net-normal.png', width = 1400, height = 1024)
z = qgraph(xcor_normal_lag3, vsize = 2, 
           graph = "sig", alpha = c(1e-8, 1e-6, 1e-3), 
           groups = grps, 
           edge.color = brewer.pal(9, 'Blues')[7], 
           details = FALSE, edge.width = 0.2)
dev.off()
