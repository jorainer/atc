####============================================================
##  Generic method.
##
##
####------------------------------------------------------------
setGeneric("atcData", function(x, columns, key, level, pattern)
    standardGeneric("atcData"))

## if(!isGeneric("listTables")){
##     setGeneric("listTables", function(x, ...)
##         standardGeneric("listTables"))
## }



setGeneric("cleanColumns", function(x, columns, ...)
    standardGeneric("cleanColumns"))
setGeneric("cleanTables", function(x, tables, ...)
    standardGeneric("cleanTables"))
setGeneric("cleanFilter", function(x, filter, ...)
    standardGeneric("cleanFilter"))
setGeneric("buildQuery", function(x, ...)
    standardGeneric("buildQuery"))
setGeneric("prefixColumns", function(x, columns, clean=TRUE, with.tables)
    standardGeneric("prefixColumns"))
setGeneric("buildJoinQuery", function(x, ...)
    standardGeneric("buildJoinQuery"))
setGeneric("buildFilterQuery", function(x, ...)
    standardGeneric("buildFilterQuery"))
setGeneric("sortTablesByDegree", function(x, ...)
    standardGeneric("sortTablesByDegree"))
setGeneric("addRequiredJoinTables", function(x, tables)
    standardGeneric("addRequiredJoinTables"))
setGeneric("getWhat", function(x, ...)
    standardGeneric("getWhat"))
setGeneric("atcs", function(x, ...)
    standardGeneric("atcs"))
