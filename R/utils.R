####============================================================
##  levelFromAtc
##
##  Function to get the level of an ATC code based on the number
##  of characters of the code.
####------------------------------------------------------------
levelFromAtc <- function(x){
    if(missing(x))
        stop("'x' is empty!")
    nchars <- nchar(x)
    level <- rep(NA, length(x))
    level[nchars == 1] <- 1
    level[nchars == 3] <- 2
    level[nchars == 4] <- 3
    level[nchars == 5] <- 4
    level[nchars == 7] <- 5
    return(level)
}


