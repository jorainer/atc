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
    return(unique(unlist(Tabs[names(Tabs) != "metadata"], use.names=FALSE)))
})

setMethod("listColumns", "AtcDb", function(x){
    Tabs <- listTables(x)
    return(unique(unlist(Tabs[names(Tabs) != "metadata"], use.names=FALSE)))
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
    .Deprecated("atcs")
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
##  atcs
##
##  That's the method to be used if we've got filters etc.
setMethod("atcs", "AtcDb", function(x, columns=listColumns(x),
                                    filter=list(), order.by="key", ...){
    startTable <- "atc"
    columns <- cleanColumns(x, columns)
    ## Get the data
    res <- getWhat(x, columns=columns, filter=filter, join="left join",
                   order.by=order.by, start.table=startTable, ...)
    return(res)
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



####
## Note: that's the central function that checks which tables are needed for the
## least expensive join!!! The names of the tables should then also be submitted
## to any other method that calls .prefixColumns (e.g. where of the Filter classes)
##
## this function checks:
## a) for multi-table attributes, selects the table with the highest degree (i.e.
##    the table connected to most other tables).
## b) pre-pend (inverse of append ;)) the table name to the attribute name.
## returns a list, names being the tables and the values being the attributes
## named: <table name>.<attribute name>
## clean: whether a cleanColumns should be called on the submitted attributes.
## with.tables: force the prefix to be specifically on the submitted tables.
##
## Return values:
## If none of the provided columns is present in the database, the
## function (given that clean=TRUE) returns NULL otherwise it returns
## a list, list names being the table names and elements the (prefixed) column names.
setMethod("prefixColumns", "AtcDb", function(x, columns, clean=TRUE, with.tables){
    if(missing(columns))
        stop("columns is empty! No columns provided!")
    ## first get to the tables that contain these columns
    Tab <- listTables(x)   ## returns the tables by degree!
    if(!missing(with.tables)){
        with.tables <- with.tables[with.tables %in% names(Tab)]
        if(length(with.tables) > 0)
            Tab <- Tab[with.tables]
        if(length(Tab)==0)
            stop("None of the tables submitted with with.tables is present in the database!")
    }
    if(clean)
        columns <- cleanColumns(x, columns)
    if(length(columns)==0){
        return(NULL)
    }
    ## Group the columns by table.
    columns.bytable <- lapply(Tab, function(z){
        return(z[z %in% columns])
    })
    ## Kick out empty tables...
    columns.bytable <- columns.bytable[lengths(columns.bytable) > 0]
    if(length(columns.bytable)==0)
        stop("No columns available!")
    have.columns <- NULL
    ## We're not re-ordering the columns, but keep them in the order of the "most important"
    ## ones.
    ## Order the tables by the number of columns they have.
    ## columns.bytable <- columns.bytable[order(lengths(columns.bytable), decreasing=TRUE)]
    ## Remove redundant columns.
    ## Loop throught the columns by table and sequentially kick out columns
    ## for the current table if they where already
    ## in a previous (more relevant) table. That way we might reduce the number of tables
    ## from which we have to fetch data and thus speed up queries (given that different tables)
    ## have similar content.
    for(i in 1:length(columns.bytable)){
        currentCols <- columns.bytable[[i]]
        keepColumns <- currentCols[!(currentCols %in% have.columns)]   ## keep those
        if(length(keepColumns) > 0){
            have.columns <- c(have.columns, keepColumns)
            ## Add the <table name>.<column name>
            columns.bytable[[i]] <- paste(names(columns.bytable)[i], keepColumns, sep=".")
        }else{
            ## Just put in the empty stuff.
            columns.bytable[[i]] <- keepColumns
        }
    }
    return(columns.bytable[lengths(columns.bytable) > 0])
})

####============================================================
##  .removePrefix
##
##  remove the prefix from the column name (i.e. the table name).
####------------------------------------------------------------
.removePrefix <- function(x, split=".", fixed=TRUE){
    if(is(x, "list")){
        ## e.g. what we get back grom .prefixColumns
        return(lapply(x, function(z){
            tmp <- unlist(strsplit(z, split=split, fixed=fixed), use.names=FALSE)
            return(tmp[length(tmp)])
        }))
    }else{
        tmp <- strsplit(x, split=split, fixed=fixed)
        return(unlist(lapply(tmp, function(z){
            return(z[length(z)])
        })))
    }
}

####============================================================
##  cleanTables
##
##  Checks if the provided table names are available in the database and removes
##  those that aren't
####------------------------------------------------------------
setMethod("cleanTables", "AtcDb", function(x, tables){
    if(missing(tables))
        return(NULL)
    gotTabs <- names(listTables(x))
    areOK <- tables %in% gotTabs
    notOK <- tables[!areOK]
    if(length(notOK) > 0)
        warning("Tables ", paste(sQuote(notOK), collapse=", "), " are not available",
                " in the database and have thus been removed.")
    tables <- tables[areOK]
    if(length(tables) == 0){
        warning("None of the provided tables are known to the database!")
        return(NULL)
    }
    return(tables)
})

####============================================================
##  cleanColumns
##
##  Checks if the provided columns are available in the database
##  and removes those that are not in the database.
##  The method returns all columns that are in the database, removes all that
##  are not present (with a warning). If none are present, it returns NULL.
setMethod("cleanColumns", "AtcDb", function(x, columns, excludes="metadata"){
    if(missing(columns))
        return(NULL)
    tabs <- listTables(x)
    availableCols <- unlist(tabs[!(names(tabs) %in% excludes)], use.names=FALSE)
    areOK <- columns %in% availableCols
    notOK <- columns[!areOK]
    if(length(notOK) > 0)
        warning("Columns ", paste(sQuote(notOK), collapse=", "), " are not available",
                " in the database and have thus been removed.")
    columns <- columns[areOK]
    if(length(columns) == 0){
        warning("None of the provided columns are known to the database!")
        return(NULL)
    }
    return(columns)
})

####============================================================
##  sortTablesByDegree
##
##  Sort the given table names by their degree, i.e. number of other
##  tables with which they are connected (via primary/foreign keys).
##  Returns a re-ordered character vector of table names.
####------------------------------------------------------------
setMethod("sortTablesByDegree", "AtcDb",
          function(x, tables=names(listTables(x))){
              ## Well, at present we just have two tables anyway...
              tableOrder <- c(atc=1, ddd=2, metadata=99)
              tables <- cleanTables(x, tables)
              return(tables[order(tableOrder[tables])])
          })

####============================================================
##  buildFilterQuery
##
##  with.tables is optional to force usage of the specified tables.
####------------------------------------------------------------
setMethod("buildFilterQuery", "AtcDb", function(x, filter, with.tables){
    filter <- cleanFilter(x, filter)
    if(missing(with.tables)){
        with.tables <- names(listTables(x))
    }
    if(length(filter) == 0)
        return("")
    query <- paste0(" where ",
                    paste(unlist(lapply(filter, where, x,
                                        with.tables=with.tables)),
                          collapse=" and "))
    return(query)
})

## only list direct joins!!!
.JOINS <- rbind(
    c("atc", "ddd", "on (atc.key=ddd.key)")
    )


####============================================================
##  addRequiredTables
##
##  Add additional tables in case the submitted ones can not be joined
##  directly.
####------------------------------------------------------------
setMethod("addRequiredJoinTables", "AtcDb", function(x, tables){
    ## Presently, all tables are joined directly, so there shouldn't be any need
    ## to add intermediate tables; yet.
    ## In case: check dbhelpers.R
    ## in mirhostgenes package, addRequiredTables function.
    return(tables)
})

####============================================================
##  cleanFilter
##
##  loops through the filters and checks if the filters are supported
##  in the present database, i.e. if their column is present in the
##  database. The method removes all filters that are not supported
##  with a respective warning message.
##  x: the SimpleCompoundDb object.
##  filter: the list of filter objects.
####------------------------------------------------------------
setMethod("cleanFilter", "AtcDb", function(x, filter){
    if(missing(filter))
        filter <- list()
    if(is(filter, "BasicFilter"))
        filter <- list(filter)
    if(is(filter, "list")){
        tabs <- listTables(x)
        if(length(filter) > 0){
            res <- lapply(filter, function(z){
                tmp <- try(column(z, db=x), TRUE)
                if(inherits(tmp, "try-error")){
                    warning("Removed filter ", class(z)[1], " as it is not supported",
                            " by the ", class(x)[1], " database.")
                    return(NULL)
                }else{
                    return(z)
                }
            })
            res <- res[lengths(res) > 0]
        }else{
            return(list())
        }
    }else{
        stop("Argument 'filter' has to be either a single filter object inheriting",
             " from 'BasicFilter' or a list of such objects!")
    }
})


####============================================================
##  buildJoinQuery
##
####------------------------------------------------------------
setMethod("buildJoinQuery", "AtcDb", function(x, columns, join="join", start.table){
    ## Get the table names and prefixed column names.
    tabs <- names(prefixColumns(x, columns))
    ## Add tables that might be needed to join the specified tables.
    tabs <- addRequiredJoinTables(x, tabs)
    ## Sort the tables.
    tabs <- sortTablesByDegree(x, tabs)
    if(!missing(start.table)){
        if(any(tabs == start.table)){
            tabs <- c(start.table, tabs[tabs != start.table])
        }
    }
    query <- tabs[1]
    previousTable <- tabs[1]
    remainingTable <- tabs[-1]
    ## Now loop through the tables and add them sequentially.
    while(length(previousTable)!=length(tabs)){
        ## Repeat until I don't have joined all tables!
        ## check which joins are available for the last table(s)
        ## Basically we're extracting those rows from the .JOINS matrix that have
        ## the previousTable in the first or second column.
        previousIdx <- which(apply(.JOINS[ , 1:2, drop=FALSE ], MARGIN=1,
                                    function(z){ any(z %in% previousTable) }))
        if(length(previousIdx) == 0)
            stop("Argh, something weird happened...")
        ## Now check if in the remaining tables there is one that could be joined to this one.
        tmp <- .JOINS[ previousIdx, , drop=FALSE ]
        remainingIdx <- which(apply(tmp[ , 1:2, drop=FALSE ], MARGIN=1,
                                     function(z){ any(z %in% remainingTable) }))
        ## add this table to the previousTable vector
        previousTable <- unique(c(previousTable,
                                  tmp[remainingIdx[1], 1:2][tmp[remainingIdx[1], 1:2]!=previousTable[length(previousTable)]]))
        remainingTable <- remainingTable[remainingTable!=previousTable[length(previousTable)]]
        query <- paste(query, join, previousTable[length(previousTable)],
                       tmp[remainingIdx[1], 3])
    }
    return(query)
})

####============================================================
##  buildQuery
##  x: AtcDb
##  filter: list of BasicFilter objects. Should already be checked by the cleanFilter function.
##  order.by: if we should add an order by to the query. The provided column will also be checked
##            if it is available in the database.
##  order.type: asc or desc.
##  skip.order.check: whether the checking of the "order.by" should be omitted (i.e. if we're submitting
##                    a more complex order.by and not just the column name.
##  join: which join should be performed. join (default), left join, left outer join.
##  start.table: from which table should the join start?
##  return.all.columns: if all columns should be returned (also filter columns etc), or just
##                      the columns passed with argument columns.
##  Returns: a character string with the query.
####------------------------------------------------------------
setMethod("buildQuery", "AtcDb",
          function(x, columns, filter=list(), order.by="", order.type="asc",
                   skip.order.check=FALSE, join="join",
                   start.table, return.all.columns=TRUE){
              resultcolumns <- columns    ## just to remember what we really want to give back
              join <- match.arg(join, c("join", "left join", "left outer join"))
              ## 1) get all column names from the filters also removing the prefix.
              filter <- cleanFilter(x, filter)
              if(length(filter) > 0){
                  ## Add the columns needed for the filter
                  filtercolumns <- unlist(lapply(filter, column, x))
                  ## Remove the prefix (column name for these)
                  filtercolumns <- .removePrefix(filtercolumns)
                  columns <- unique(c(columns, filtercolumns))
              }
              ## 2) Get all column names for the order.by, check if they exist in
              ##    the database:
              if(order.by!=""){
                  ## if we have skip.order.check set we use the order.by as is.
                  if(!skip.order.check){
                      order.by <- unlist(strsplit(order.by, split=",", fixed=TRUE))
                      order.by <- gsub(order.by, pattern=" ", replacement="", fixed=TRUE)
                      ## Check if order.by are valid columns.
                      order.by <- cleanColumns(x, order.by)
                      if(length(order.by)==0){
                          order.by <- ""
                      }else{
                          columns <- unique(c(columns, order.by))
                          order.by <- paste(unlist(prefixColumns(x, columns=order.by,
                                                                 with.tables=names(prefixColumns(x, columns)))
                                                   , use.names=FALSE), collapse=", ")
                      }
                  }
              }else{
                  order.by <- ""
              }
              ## Note: order by is now a vector!!!
              ## columns are now all columns that we want to fetch or that we need to
              ## filter or to sort.
              ## 3) check which tables we need for all of these columns:
              need.tables <- names(prefixColumns(x, columns))
              ##
              ## Now we can begin to build the query parts!
              ## a) the query part that joins all required tables.
              joinquery <- buildJoinQuery(x, columns=columns, join=join,
                                          start.table=start.table)
              ## b) the filter part of the query
              filterquery <- buildFilterQuery(x, filter=filter, with.tables=need.tables)
              ## c) the order part of the query
              if(order.by!=""){
                  orderquery <- paste(" order by", order.by, order.type)
              }else{
                  orderquery <- ""
              }
              ## And finally build the final query
              if(return.all.columns){
                  resultcolumns <- columns
              }
              finalquery <- paste0("select distinct ",
                                   paste(unlist(prefixColumns(x,
                                                              resultcolumns,
                                                              with.tables=need.tables),
                                                use.names=FALSE), collapse=","),
                                   " from ",
                                   joinquery,
                                   filterquery,
                                   orderquery
                                   )
              return(finalquery)
          })

####============================================================
##  getWhat.
##
## That's the main entrance point for all stuff querying the database.
## just to add another layer; basically just calls buildQuery and executes the query
## return.all.columns: returns also columns added because of the filters.
setMethod("getWhat", "AtcDb",
          function(x, columns, filter=list(), order.by="", order.type="asc",
                   skip.order.check=FALSE, join="join", start.table,
                   return.all.columns=TRUE){
              if(missing(columns)){
                  tabs <- listTables(x)
                  columns <- unlist(tabs[names(tabs) != "metadata"], use.names=FALSE)
              }
              Q <- buildQuery(x=x, columns=columns, filter=filter, order.by=order.by,
                              order.type=order.type,
                              skip.order.check=skip.order.check, join=join,
                              start.table=start.table, return.all.columns=return.all.columns)
              return(dbGetQuery(dbconn(x), Q))
})

