context("Accessing data from atc database")

test_that("use atcs", {
    ## trivial.
    res <- atcs(atc, columns = c("key", "name"))
    expect_identical(colnames(res), c("key", "name"))


    ## Use combinations of filter.
    kf <- KeyFilter("C01%", condition = "startsWith")
    af <- AdminrouteFilter(c("O", "P"))
    res <- atcs(atc, columns=c("key", "name"), filter=list(kf, af))
    expect_true(all(res$administration_route %in% c("O", "P")))

    af2 <- AdminrouteFilter("Inh%", condition = "startsWith")
    res <- atcs(atc, columns=c("key", "name"), filter=af2)
})
