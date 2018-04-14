library(tidyverse)

# A random 0 or 1 square matrix of dimensions n*m
test_matrix <- function(n, m = n){
  matrix(sample(0:1, size = n*m, replace = TRUE), nrow = n, ncol = m)
}

low_matrix <- function(n, x = 1){
  matrix(rep(x, n^2), nrow = n)
}

#' Sum the sub-matrix in my_matrix for sub-matrix of size k
#' @importFrom purrr
#' @return A matrix of sums, where each coordinate is sum of matrix size k in position in original matrix
f_subset <- function(my_matrix, k, f = sum){
  my_dim <- dim(my_matrix)
  k_n <- (k-1)
  along <- my_dim[1] - k_n
  down  <- my_dim[2] - k_n
  map(1:along, 
      ~ map_dbl(1:down, 
                function(y) f(my_matrix[.x:(.x+k_n), y:(y+k_n)])
                )
      ) %>% 
    unlist %>% matrix(ncol = along, byrow = TRUE) / k^2

}

# a named vector of entropy for k
# calculates entropy per k
all_ks <- function(my_matrix){
  my_dim <- dim(my_matrix)
  along <- 1:(min(my_dim))
  along <- setNames(along, along)
  
  map_dbl(along, 
          ~ sum(log2(f_subset(my_matrix, .) / sum(f_subset(my_matrix, .))))*(-1))
}

# plots each matrix with k and entropy
#' @param k TRUE if all, or a vector of which k to plot
plot_ks <- function(my_matrix, k = TRUE){
  my_dim <- dim(my_matrix)

  along <- all_ks(my_matrix)
  along <- along[k]
  
  par(mfrow = c(ceiling(sqrt(my_dim[1])), ceiling(sqrt(my_dim[2]))),
      mar = c(1.5,1.5,1.5,1.5))
  
  walk(names(along), 
       function(x){
         aa <- along[[x]]
         image(f_subset(my_matrix, as.numeric(x)),
                col = RColorBrewer::brewer.pal(3, "Oranges"))
         title(main = paste("k=",x, " S = ", round(aa, 2)))
         
       })
  
  along
}


## order matrices from lowest to highest - must be same dims
entropy_order <- function(matrices){
  
  matrices %>% 
    as_entropy_df %>% 
    arrange_all() %>% 
    pull(index) %>% 
    matrices[.]
  
}

# creates a dataframe of entropies for matricies passed in
as_entropy_df <- function(matrices){
  matrices %>% 
    map(all_ks) %>% 
    reduce(bind_rows) %>% 
    mutate(index = row_number())
}

dedupe_matrices <- function(matrices){
  
  matrices[!duplicated(matrices)]

}

