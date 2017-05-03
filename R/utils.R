####============================================================
##  levelFromAtc
##
##  Function to get the level of an ATC code based on the number
##  of characters of the code.
####------------------------------------------------------------
levelFromAtc <- function(x){
    if(missing(x))
        stop("'x' is empty!")
    nchars <- nchar(x)
    level <- rep(NA, length(x))
    level[nchars == 1] <- 1
    level[nchars == 3] <- 2
    level[nchars == 4] <- 3
    level[nchars == 5] <- 4
    level[nchars == 7] <- 5
    return(level)
}


.AnnotationFilterClassNames <- function(x) {
    classes <- lapply(x, function(z) {
        if (is(z, "AnnotationFilterList"))
            return(.AnnotationFilterClassNames(z))
        class(z)
    })
    unlist(classes, use.names = FALSE)
}

.where <- function(object){
    Vals <- .valueForAtc(object)
    return(paste(.conditionForAtc(object), Vals))
}

#' Utility function to map the condition of an AnnotationFilter to the SQL
#' condition to be used in the EnsDb database.
#'
#' @param x An \code{AnnotationFilter}.
#'
#' @return A character representing the condition for the SQL call.
#' @noRd
.conditionForAtc <- function(x) {
    cond <- condition(x)
    if (length(value(x)) > 1) {
        if (cond == "==")
            cond <- "in"
        if (cond == "!=")
            cond <- "not in"
    }
    if (cond == "==")
        cond <- "="
    if (cond %in% c("startsWith", "endsWith"))
        cond <- "like"
    cond
}

#' Single quote character values, paste multiple values and enclose in quotes.
#'
#' @param x An \code{AnnotationFilter} object.
#' @noRd
.valueForAtc <- function(x) {
    vals <- unique(value(x))
    if (is(x, "CharacterFilter")) {
        vals <- sQuote(gsub(unique(vals), pattern = "'", replacement = "''"))
    }
    if (length(vals) > 1)
        vals <- paste0("(",  paste0(vals, collapse = ","), ")")
    ## Process the like/startsWith/endsWith
    if (condition(x) == "startsWith")
        vals <- paste0("'", unique(x@value), "%'")
    if (condition(x) == "endsWith")
        vals <- paste0("'%", unique(x@value), "'")
    vals
}

## #' That's to build the standard query from an AnnotationFilter for EnsDb.
## #'
## #' @param x An \code{AnnotationFilter}.
## #' @noRd
## .queryForAtc <- function(x) {
##     paste(field(x), .conditionForAtc(x), .valueForAtc(x))
## }

## #' This is a slightly more sophisticated function that does also prefix the
## #' columns.
## #' @noRd
## .queryForAtcWithTables <- function(x, db, tables = character()) {
##     clmn <- field(x)
##     if (!missing(db)) {
##         if (length(tables) == 0)
##             tables <- names(listTables(db))
##         clmn <- unlist(prefixColumns(db, clmn, with.tables = tables))
##     }
##     res <- paste(clmn, .conditionForAtc(x), .valueForAtc(x))
##     ## cat("  ", res, "\n")
##     return(res)
## }
