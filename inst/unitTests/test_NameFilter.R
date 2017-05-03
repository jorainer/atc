
test_NameFilter_constructor <- function(){
    nf <- NameFilter("sodium fluoride")
    checkEquals(atc:::where(nf), "= 'sodium fluoride'")
    checkEquals(atc:::where(nf, atc), "atc.name = 'sodium fluoride'")

    checkEquals(atc:::column(nf), "name")
    checkEquals(atc:::column(nf, atc), "atc.name")
}

test_NameFilterQuery <- function(){
    nf <- NameFilter("sodium fluoride")
    res <- atcs(atc, filter=nf)
    checkEquals(res$key, c("A01AA01", "A12CD01"))

    ## nf <- NameFilter("%respiratory%", condition="like")
    ## res <- atcs(atc, filter=nf)
    nf <- NameFilter("resp", condition="startsWith")
    res <- atcs(atc, filter=nf)
}


