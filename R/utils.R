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

.levelIdx <- c(`1` = 1, `2` = 3, `3` = 4, `4` = 5, `5` = 7)

#' @title Utilities to work with ATC codes
#' @description \code{atcLevel} determines the ATC \emph{level} for the
#'     provided ATC code. Note that the level is purely determined by the
#'     number of characters. The function does not evaluate whether the
#'     provided key represents a valid ATC key/code.
#'     See \code{\link{AtcDb}} for an overview of the various levels.
#'
#' @param x For \code{atcLevel} and \code{toAtcLevel}: \code{character}
#'     representing ATC code(s). 
#'
#' @return For \code{atcLevel}: an \code{integer} of length 1 representing the
#'     ATC level. The function returns \code{NA} if the length of the ATC code
#'     does not match any of the levels (i.e. the length of the code is not
#'     equal to 1, 3, 4, 5 or 7).
#' 
#' @author Johannes Rainer
#' 
#' @rdname atc-utils
#'
#' @seealso \code{\link{AtcDb}} for a listing and description of ATC levels.
#' 
#' @examples
#'
#' ## Get the level for given ATC codes
#' atcLevel(c("C07AB07", "A12AX"))
#'
#' ## Invalid ATC code
#' atcLevel("AA")
atcLevel <- function(x) {
    levelFromAtc(x)
}

## for a given ATC code, return the lowe level ATC.
#' @description \code{toAtcLevel} transforms the provided ATC code(s) to a lower
#'     level code. The function returns `NA` for codes that have a lower
#'     level than the level specified with parameter \code{level}. 
#'
#' @param level \code{integer} with the level to which the ATC code(s) should be
#'     transformed. Can also have \code{length(level) > 1} in which case each
#'     ATC code is converted into an ATC code for each level.
#' 
#' @return \code{toAtcLevel}: if \code{length(level) == 1}: returns a
#'     \code{character} of the same length then \code{x}. If
#'     \code{length(level) > 1}: returns a \code{matrix} of ATC codes, each
#'     column corresponding to elements in \code{x}, each row to ATC levels.
#'
#' @rdname atc-utils
#'
#' @examples
#'
#' ## Transform an ATC code to a lower level.
#' toAtcLevel("A01", level = 1)
#'
#' ## Transform one ATC code into two different levels
#' toAtcLevel("A01CD04", level = c(1, 3))
#' 
#' ## Transform two codes.
#' toAtcLevel(c("A01CD", "A01CD04"), level = 1)
#'
#' ## Transform two codes in 3 levels
#' toAtcLevel(c("A01CD", "A01CD04"), level = c(1, 3, 2))
toAtcLevel <- function(x, level = 1) {
    levelLength <- length(level)
    if (any(level < 1 | level > 5))
        stop("'level' has to be between 1 and 5")
    res <- sapply(x, function(z, level) {
        .validAtcLength(z)
        isLevel <- atcLevel(z)
        if (any(isLevel < level))
            NA_character_
        else
            substr(rep_len(z, levelLength), start = rep_len(1, levelLength),
                   stop = .levelIdx[level])
    }, level = level, USE.NAMES = TRUE, simplify = TRUE)
    if (any(is.na(res)))
        warning("ATC codes for ", sum(is.na(res)), " elements are lower than",
                " 'level'. Returning 'NA' for them.")
    res
}

#' @description Simple check that the length of an ATC code is valid.
#'
#' @param x \code{character(1)}.
#' @noRd
.validAtcLength <- function(x) {
    if (!(nchar(x) %in% c(1, 3, 4, 5, 7)))
        stop("Provided ATC code has invalid length")
    TRUE
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
    if (cond %in% c("startsWith", "endsWith", "contains"))
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
    if (condition(x) == "contains")
        vals <- paste0("'%", unique(x@value), "%'")
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
