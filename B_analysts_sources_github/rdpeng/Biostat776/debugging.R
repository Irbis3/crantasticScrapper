printmessage <- function(x) {
        if(x > 0)
                print("x is greater than zero")
        else 
                print("x is less than or equal to zero")
        invisible(x)
}

printmessage2 <- function(x) {
        if(is.na(x))
                print("x is a missing value!")
        else if(x > 0)
                print("x is greater than zero")
        else 
                print("x is less than or equal to zero")
        invisible(x)
}

checkvalue <- function(x) {
        x1 <- log(x)
        printmessage2(x)
}

