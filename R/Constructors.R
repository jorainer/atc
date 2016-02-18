####============================================================
##  Constructor for AtcDb.
AtcDb <- function(x){
    options(useFancyQuotes=FALSE)
    if(missing(x)){
        stop("No sqlite file provided!")
    }
    lite <- dbDriver("SQLite")
    con <- dbConnect(lite, dbname = x, flags=SQLITE_RO)
    atc <- new("AtcDb", atcdb=con)
    theTables <- .doListTables(atc)
    atc@tables <- theTables
    return(atc)
}

.validateAtcDb <- function(object){
    if(!is.null(object@atcdb)){
        con <- object@atcdb
        requiredTables <- c("atc", "ddd", "metadata")
        tables <- dbListTables(con)
        if(!all(requiredTables %in% tables)){
            errStr <- paste0("The SQLite database does not provide the required",
                             " tables 'atc', 'ddd' and 'metadata'!")
            return(errStr)
        }
    }
    return(TRUE)
}
setValidity("AtcDb", .validateAtcDb)
setMethod("initialize", "AtcDb", function(.Object, ...){
    OK <- .validateAtcDb(.Object)
    if(is(OK, "character"))
        stop(OK)
    callNextMethod(.Object, ...)
})


