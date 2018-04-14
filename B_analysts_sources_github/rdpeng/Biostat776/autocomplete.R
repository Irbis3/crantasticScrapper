load("words.rda")

autocomplete <- function(letters, data = words) {
        ## If given a phrase, find the last word
        r <- regexpr("[a-z0-9']+$", letters, perl = TRUE)
        
        ## Get letters representing incomplete word
        letters <- regmatches(letters, r)
        
        ## Create a regular expression
        regex <- paste0("^", letters)
        
        ## Find the regex in the word list
        idx <- grep(regex, data, perl = TRUE)
        if(length(idx) > 0)
                data[idx[1]]
        else
                letters
}


