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
    checkEquals(sort(ks), sort(df$key))
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
    checkEquals(l1, df[df$level==1, ])
    l2 <- atcData(atc, level=2:4)
    checkEquals(l2, df[df$level %in% 2:4, ])
    Test <- atcData(atc, level=2:4, key=c("A01"))
    checkEquals(Test, df["A01", ])
    ## Now with some pattern matches...
    idxde <- grep(df$name_de, pattern="cardio", ignore.case=TRUE)
    idxen <- grep(df$name_en, pattern="cardio", ignore.case=TRUE)
    df[c(idxde, idxen), ]
    Test <- atcData(atc, pattern="cardio")
    checkEquals(sort(rownames(Test)), sort(rownames(df)[c(idxde, idxen)]))
}




