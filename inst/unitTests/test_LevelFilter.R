
test_LevelFilter_constructor <- function(){
    lf <- LevelFilter(1)
    checkException(LevelFilter("sd"))
    checkEquals(where(lf), "= 1")
    checkEquals(where(lf, atc), "atc.level = 1")

    checkEquals(column(lf), "level")
    checkEquals(column(lf, atc), "atc.level")
}

test_LevelFilterQuery <- function(){
    lf <- LevelFilter(1)
    res <- atcs(atc, filter=lf)
    checkTrue(all(res$level == 1))
    library(RSQLite)
    resM <- dbGetQuery(dbconn(atc), "select * from atc where level = 1 order by key asc")
    checkEquals(res$key, resM$key)

    lf <- LevelFilter(1:3, condition="=")
    res <- atcs(atc, filter=lf)
    checkTrue(all(res$level %in% 1:3))
    resM <- dbGetQuery(dbconn(atc), "select * from atc where level in (1, 2, 3) order by key asc")
    checkEquals(res$key, resM$key)
}


test_combineFilters <- function(){
    res <- atcs(atc, filter=list(LevelFilter(2),
                                 NameFilter("%respi%", condition="like")))
    checkEquals(res$key, "R07")
}

