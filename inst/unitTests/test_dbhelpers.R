
test_prefixColumns <- function(){
    Res <- atc:::prefixColumns(atc, "key")
    checkEquals(unname(unlist(Res)), "atc.key")
    Res <- atc:::prefixColumns(atc, "key", with.tables="ddd")
    checkEquals(unname(unlist(Res)), "ddd.key")
    Res <- atc:::prefixColumns(atc, "unit")
    checkEquals(unname(unlist(Res)), "ddd.unit")
    checkException(atc:::prefixColumns(atc, "unit", with.tables="atc"))

    Res <- atc:::prefixColumns(atc, listColumns(atc))
}

test_cleanTables <- function(){
    Res <- atc:::cleanTables(atc, c("bla", "blu", "ddd", "atc"))
    checkEquals(Res, c("ddd", "atc"))
}

test_cleanColumns <- function(){
    Res <- atc:::cleanColumns(atc, c("bla, ", "unit", "key"))
    checkEquals(Res, c("unit", "key"))
}

test_sortTablesByDegree <- function(){
    Res <- atc:::sortTablesByDegree(atc)
}

test_buildFilterQuery <- function(){
}

test_addRequiredJoinTables <- function(){
    Res <- atc:::addRequiredJoinTables(atc, "ddd")
    checkEquals(Res, "ddd")
}

test_buildJoinQuery <- function(){
    Res <- atc:::buildJoinQuery(atc, columns=c("name", "unit"))
    checkEquals(Res, "atc join ddd on (atc.key=ddd.key)")
    Res <- atc:::buildJoinQuery(atc, columns=c("key", "unit"))
    checkEquals(Res, "atc join ddd on (atc.key=ddd.key)")
}

test_buildQuery <- function(){
    Res <- atc:::buildQuery(atc, columns=c("name", "key", "unit"))
    checkEquals(Res, paste0("select distinct atc.key,atc.name,ddd.unit",
                            " from atc join ddd on (atc.key=ddd.key)"))
    Res <- atc:::buildQuery(atc, columns=c("name", "key", "unit"), order.by="unit")
    checkEquals(Res, paste0("select distinct atc.key,atc.name,ddd.unit from",
                            " atc join ddd on (atc.key=ddd.key) order by ddd.unit asc"))

    ## With a filter.
    kf <- KeyFilter("R01")
    Res <- atc:::buildQuery(atc, columns=c("name", "key", "unit"), filter=kf)
    checkEquals(Res, paste0("select distinct atc.key,atc.name,ddd.unit from atc",
                            " join ddd on (atc.key=ddd.key) where atc.key = 'R01'"))
}

test_getWhat <- function(){
    ## Heck! this data base sucks.
    library(RSQLite)
    TheQ <- atc:::buildQuery(atc, listColumns(atc), join="left join")
    TheQ2 <- atc:::buildQuery(atc, listColumns(atc), join="left join")
    ResQ <- RSQLite::dbGetQuery(dbconn(atc), TheQ)
    ResQ2 <- dbGetQuery(dbconn(atc), TheQ2)
    Res <- atc:::getWhat(atc, join="left join")
    Res2 <- atcData(atc)
    checkEquals(Res[order(Res$key), colnames(Res2)], Res2[order(Res2$key),])
}

