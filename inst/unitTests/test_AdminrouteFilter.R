
test_AdminrouteFilter_constructor <- function(){
    af <- AdminrouteFilter("O")
    checkEquals(atc:::where(af), "= 'O'")
    checkEquals(atc:::where(af, atc), "ddd.administration_route = 'O'")

    checkEquals(atc:::column(af), NULL)
    checkEquals(atc:::column(af, atc), "ddd.administration_route")
}

test_AdminrouteFilterQuery <- function(){
    af <- AdminrouteFilter("O")
    res <- atcs(atc, filter = af)
    checkTrue(all(res$administration_route == "O"))
    library(RSQLite)
    resM <- dbGetQuery(
        dbconn(atc),
        "select * from ddd where administration_route = 'O' order by key asc")
    checkEquals(res$key, resM$key)

    af <- AdminrouteFilter(c("O", "P"), condition = "==")
    res <- atcs(atc, filter=af)
    checkTrue(all(res$administration_route %in% c("O", "P")))
    resM <- dbGetQuery(dbconn(atc), "select * from ddd where administration_route in ('O', 'P') order by key asc")
    checkEquals(res$key, resM$key)

    res_2 <- atcs(atc, filter = ~ adminroute == c("O", "P"))
    checkEquals(res, res_2)
}


test_combineFilters <- function(){
    ## Get all oral GCs.
    res <- atcs(atc, filter=list(KeyFilter("H02AB", condition = "startsWith"),
                                 AdminrouteFilter("O", condition="==")))
    checkEquals(grep(res[, "key"], pattern = "^H02AB"), 1:nrow(res))
    checkTrue(all(res[, "administration_route"] == "O"))
}

