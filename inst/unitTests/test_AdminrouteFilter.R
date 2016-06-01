
test_AdminrouteFilter_constructor <- function(){
    af <- AdminrouteFilter("O")
    checkEquals(where(af), "= 'O'")
    checkEquals(where(af, atc), "ddd.administration_route = 'O'")

    checkEquals(column(af), NULL)
    checkEquals(column(af, atc), "ddd.administration_route")
}

test_AdminrouteFilterQuery <- function(){
    af <- AdminrouteFilter("O")
    res <- atcs(atc, filter=af)
    checkTrue(all(res$administration_route == "O"))
    library(RSQLite)
    resM <- dbGetQuery(dbconn(atc), "select * from ddd where administration_route = 'O' order by key asc")
    checkEquals(res$key, resM$key)

    af <- AdminrouteFilter(c("O", "P"), condition="=")
    res <- atcs(atc, filter=af)
    checkTrue(all(res$administration_route %in% c("O", "P")))
    resM <- dbGetQuery(dbconn(atc), "select * from ddd where administration_route in ('O', 'P') order by key asc")
    checkEquals(res$key, resM$key)
}


test_combineFilters <- function(){
    ## Get all oral GCs.
    res <- atcs(atc, filter=list(KeyFilter("H02AB%", condition="like"),
                                 AdminrouteFilter("O", condition="=")))
}

