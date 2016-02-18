####============================================================
##  show
##
##  show method for AtcDb
####------------------------------------------------------------
setMethod("show", "AtcDb", function(object){
    cat(class(object), " object:\n", sep="")
    metad <- dbGetQuery(dbconn(object), "select * from metadata;")
    cat("| metadata: \n")
    for(i in 1:nrow(metad)){
        cat("| o", metad[i, "name"], ": ", metad[i, "value"],"\n")
    }
    cat("| data summary:\n")
    res <- dbGetQuery(dbconn(object), "select * from atc;")
    Tab <- table(res$level)
    for(i in 1:length(Tab)){
        cat("| o level", i, ":", as.numeric(Tab[as.character(i)]),
            "entries\n")
    }
})


####============================================================
##  dbconn
##
##  getter method for the database connection.
####------------------------------------------------------------
setMethod("dbconn", "AtcDb", function(x){
    return(x@atcdb)
})

####============================================================
##  keys
##
##  returns all ATC codes stored in the database.
####------------------------------------------------------------
setMethod("keys", "AtcDb", function(x, level, ...){
    wQuery <- .levelConditionQuery(level)
    if(length(wQuery) > 0)
        wQuery <- paste0("where ", wQuery)
    Q <- paste0("select key from atc ", wQuery, " order by key;")
    res <- dbGetQuery(dbconn(x), Q)
    return(res[, "key"])
})

####============================================================
##  columns
##
##  returns all columns available  in the database.
####------------------------------------------------------------
setMethod("columns", "AtcDb", function(x){
    Tabs <- listTables(x)
    return(unique(sort(unlist(Tabs[names(Tabs) != "metadata"], use.names=FALSE))))
})

####============================================================
##  listTables
##
##  lists the database tables
####------------------------------------------------------------
setMethod("listTables", "AtcDb", function(x, ...){
    if(length(x@tables) > 0)
        return(x@tables)
    ## Else: query:
    return(.doListTables)
})
.doListTables <- function(x){
    con <- x@atcdb
    Tables <- dbListTables(con)
    theTables <- vector("list", length(Tables))
    for(i in 1:length(Tables)){
        theTables[[i]] <- colnames(dbGetQuery(con,
                                              paste0("select * from ",
                                                     Tables[i],
                                                     " limit 1;")))
    }
    names(theTables) <- Tables
    return(theTables)
}

####============================================================
##  as.data.frame
##
##
####------------------------------------------------------------
setMethod("as.data.frame", "AtcDb", function(x, ...){
    cols <- unlist(.checkColumns(x), use.names=FALSE)
    Res <- dbGetQuery(dbconn(x),
                      paste0("select ",
                             paste0(cols, collapse=","),
                             " from atc left join ddd on (atc.key=ddd.key) order by atc.key;"))
    ## setting rownames
    ## rownames(Res) <- Res$key
    return(Res)
})

####============================================================
##  atcData
##
##  Extract atc data.
##  Use: column to select what to export.
##  Restrict by:
##  o level (do a "in" on level),
##  o pattern (do a "LIKE" on name),
##  o key: the ATC code (do a "in" on key).
setMethod("atcData", "AtcDb", function(x, columns, key, level, pattern){
    ## Define the tables based on the supplied columns.
    if(missing(columns))
        columns <- NULL
    cols <- .checkColumns(x, columns)
    ## cols is a list!
    sQuery <- paste0("select ", paste0(unlist(cols, use.names=FALSE), collapse=","))
    jQuery <- .buildJoinQuery(names(cols))
    kQuery <- .keyConditionQuery(key)
    lQuery <- .levelConditionQuery(level)
    pQuery <- .patternConditionQuery(pattern)
    ## Add a "where" if kQuery or lQuery != ""
    query <- c(sQuery, jQuery)
    if(!is.null(kQuery) | !is.null(lQuery) | !is.null(pQuery)){
        query <- c(query, "where", paste0(c(kQuery, lQuery, pQuery), collapse=" and "))
    }
    query <- c(query, "order by atc.key")
    ## return(paste0(query, collapse=" "))
    Res <- dbGetQuery(dbconn(x), paste0(query, collapse=" "))
    ## if(any(colnames(Res) == "key"))
    ##     rownames(Res) <- Res$key
    return(Res)
})

####============================================================
##  children
##
##  Get all more specific ATC codes for the specified ATC. Returns
##  always a list
####------------------------------------------------------------


####============================================================
##  parents
##
##  Get all less specific ATC codes for the specified ATC. Returns
##  always a list
####------------------------------------------------------------



####============================================================
##       Internal (not exported) functions.

####============================================================
##  .checkLevel
##
##  Check whether the supplied level is valid. Throws an error
##  or just runs.
####------------------------------------------------------------
.checkLevel <- function(level){
    notAllowed <- !(level %in% 1:5)
    if(any(notAllowed))
        stop("ATC levels ", paste0(level[notAllowed], collapse=", "),
             " are not allowed! Levels have to be numbers between 1 and 5.")
    return(level[!notAllowed])
}

####============================================================
##  .levelConditionQuery
##
##  constructs the query to subset on levels.
####------------------------------------------------------------
.levelConditionQuery <- function(level, condition="in"){
    if(!missing(level)){
        level <- .checkLevel(level)
        return(paste0("level ", condition,
                      " (", paste0(level, collapse=","),")"))
    }else{
        return(NULL)
    }
}

####============================================================
##  .keyConditionQuery
##
##  constructs the query to subset on keys (atc codes).
####------------------------------------------------------------
.keyConditionQuery <- function(key, condition="in", tables=c("atc", "ddd")){
    if(!missing(key)){
        if(condition == "in"){
            return(paste0("atc.key in (", paste0(sQuote(key), collapse=","), ")"))
        }
    }else{
        return(NULL)
    }
}

####============================================================
##  .patternConditionQuery
##
##  constructs the query to subset by pattern match.
####------------------------------------------------------------
.patternConditionQuery <- function(x){
    ## Append a "collate nocase" to make the seach case insensitive.
    ## Wrap the query into (), collapse LIKE queries on x by "or"
    if(missing(x)){
        return(NULL)
    }else{
        ## Wrap % around the pattern...
        x <- sQuote(paste0("%", x, "%"))
        tmp <- paste("atc.name like", x, "collate nocase")
        return(tmp)
        ## tmp <- c(paste("atc.name_de like", x), paste("atc.name_en like", x))
        ## tmp <- paste(tmp, collapse=" or ")
        ## return(paste0("(", tmp, ") collate nocase"))
    }
}

####============================================================
##  .checkColumns
##
##  check the columns and define the tables from which to return values
##  based on them. Returns a list (!) of column names, with the names of
##  the list being the table names. Note that the column names have also
##  been "sanitized" for the database table (i.e. <table>.<column>).
####------------------------------------------------------------
.checkColumns <- function(x, columns=NULL){
    haveTables <- listTables(x)
    tableDf <- cbind(table=rep(names(haveTables), unlist(lapply(haveTables, length))),
                     column=unlist(haveTables, use.names=FALSE))
    ## Exclude the metadata table. Eventually we would need to re-order the df...
    tableDf <- tableDf[tableDf[, 1] != "metadata", ]
    allowedCols <- unique(tableDf[, 2])
    if(is.null(columns))
        columns <- allowedCols
    notAllowed <- columns[!(columns %in% allowedCols)]
    columns <- columns[columns %in% allowedCols]
    if(length(columns) == 0)
        stop("None of the submitted columns are present in the database!")
    if(length(notAllowed) > 0){
        warning("Columns ", paste0(notAllowed, ", "), " not present in ",
                "the database. These have been removed.")
    }
    ## Use match to select the first occurence in case we have two columns with
    ## the same name.
    idx <- match(columns, tableDf[, 2])
    tableDf <- tableDf[idx, , drop=FALSE]
    columns <- paste0(tableDf[, 1], ".", tableDf[, 2])
    return(split(columns, f=tableDf[, 1]))
}

####============================================================
##  .buildJoinQuery
##
##  Builds the join query in case that more than one table is specified.
##  WARNING: we're not doing any checks on the submitted tables!
####------------------------------------------------------------
.buildJoinQuery <- function(x){
    ## Do it manually...
    retr <- "from"
    if(length(x) == 1){
        retr <- c(retr, x)
    }else{
        if(any(x == "atc") & any(x == "ddd")){
            retr <- c(retr, paste0("atc left join ddd on (atc.key=ddd.key)"))
        }
        ## Eventual additional joins...
    }
    if(length(retr) == 1){
        stop("Don't know how to join tables ",
             paste0(x, collapes=","), "!")
    }
    return(paste(retr, collapse=" "))
}

## what do I want: have ATC codes, want to get the atcLevel codes for them.
## atcData
## atcLevel <- function()

##setMethod("atcCodes") -> return

## What else, data? atcLevel? that returns atcName (de, en)? as.data.frame
