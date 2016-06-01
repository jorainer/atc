
test_NameFilter_constructor <- function(){
    nf <- NameFilter("sodium fluoride")
    checkEquals(where(nf), "= 'sodium fluoride'")
    checkEquals(where(nf, atc), "atc.name = 'sodium fluoride'")

    checkEquals(column(nf), "name")
    checkEquals(column(nf, atc), "atc.name")
}

test_NameFilterQuery <- function(){
    nf <- NameFilter("sodium fluoride")
    res <- atcs(atc, filter=nf)
    checkEquals(res$key, c("A01AA01", "A12CD01"))

    nf <- NameFilter("%respiratory%", condition="like")
    res <- atcs(atc, filter=nf)
}


