# Create a simple distance matrix
# Taken from Johnson and Wichern Applied Multivariate Statistical Analysis p682

d = c(9, 3, 6, 11, 7, 5, 10, 9, 2, 8)
D = matrix(0, 5, 5)
D[row(D) > col(D) ] = d
D = D + t(D)

dimnames(D) = list(1:nrow(D), 1:nrow(D))

