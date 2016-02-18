detach("package:atc", unload=TRUE)
library(atc)
library(RUnit)
####============================================================
##  Simple tests for the database.
##
##
####------------------------------------------------------------

test_Atc <- function(){
    atc
    checkTrue(validObject(atc))

    ## Extract all the data.
    df <- as.data.frame(atc)

    ## keys
    ks <- keys(atc)
    checkEquals(sort(ks), sort(unique(df$key)))
    l1 <- keys(atc, level=1)
    checkEquals(sort(l1), sort(df[df$level == 1, "key"]))
    l2 <- keys(atc, level=2)
    checkEquals(sort(l2), sort(df[df$level == 2, "key"]))
    l123 <- keys(atc, level=1:3)
    checkEquals(sort(l123), sort(df[df$level %in% 1:3, "key"]))
    checkException(keys(atc, level=9))
}

test_atcData <- function(){
    ## Get everything.
    Test <- atcData(atc)
    df <- as.data.frame(atc)
    checkEquals(Test, df)

    ## Check some more stuff, selected entries etc.
    l1 <- atcData(atc, level=1)
    dfsub <- df[df$level == 1, ]
    rownames(dfsub) <- NULL
    checkEquals(l1, dfsub)
    l2 <- atcData(atc, level=2:4)
    dfsub <- df[df$level %in% 2:4, ]
    rownames(dfsub) <- NULL
    checkEquals(l2, dfsub)
    Test <- atcData(atc, level=2:4, key=c("A01"))
    dfsub <- df[df$key == "A01" & df$level %in% 2:4, ]
    rownames(dfsub) <- NULL
    checkEquals(Test, dfsub)
    ## Now with some pattern matches...
    idxen <- grep(df$name, pattern="cardio", ignore.case=TRUE)
    dfsub <- df[idxen, ]
    rownames(dfsub) <- NULL
    Test <- atcData(atc, pattern="cardio")
    checkEquals(Test, dfsub)
}




