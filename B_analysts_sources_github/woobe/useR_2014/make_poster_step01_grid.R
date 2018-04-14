## =============================================================================
## Mini script - set grid
## =============================================================================

row_pixel <- c("sep0",
               "header",
               "sep1",
               "sub_header",
               "sep2",
               "t_main",
               "c_main",
               "sep3",
               "t_sub1",
               "p_sub1",
               "c_sub1",
               "sep4",
               "t_sub2",
               "p_sub2",
               "c_sub2",
               "sep5",
               "footer",
               "sep6")

set_pixel <- data.frame(define = matrix(NA, nrow = length(row_pixel), ncol = 1),
                        start = matrix(NA, nrow = length(row_pixel), ncol = 1),
                        end = matrix(NA, nrow = length(row_pixel), ncol = 1))
rownames(set_pixel) <- row_pixel

## Change these
set_pixel["sep0", 1] <- 20
set_pixel["sep1", 1] <- 20
set_pixel["sep2", 1] <- 20
set_pixel["sep3", 1] <- 20
set_pixel["sep4", 1] <- 20
set_pixel["sep5", 1] <- 20
set_pixel["sep6", 1] <- 20

set_pixel["header", 1] <- 200
set_pixel["sub_header", 1] <- 120
set_pixel["t_main", 1] <- 100
set_pixel["c_main", 1] <- 600
set_pixel["t_sub1", 1] <- 80
set_pixel["p_sub1", 1] <- 100
set_pixel["c_sub1", 1] <- 200
set_pixel["t_sub2", 1] <- 80
set_pixel["p_sub2", 1] <- 100
set_pixel["c_sub2", 1] <- 200
set_pixel["footer", 1] <- 80


## Update set_pixel
set_pixel["sep0", 2] <- 1
set_pixel["sep0", 3] <- set_pixel["sep0", 1]

for (n_row in 2:nrow(set_pixel)) {
  set_pixel[n_row, 2] <- set_pixel[(n_row-1), 3] + 1
  set_pixel[n_row, 3] <- set_pixel[n_row, 2] + set_pixel[n_row, 1] - 1
}

## Print it out (too see if it matches 2000 pixel)
print(set_pixel)

