####============================================================
##  crunchWHOCC
##
##  Extract the ATC data directly from the WHOCC web page by recursively
##  querying the web page.
##  The function returns 3 data.frames: atc, ddd and metadata (these are the 3
##  tables on which we can build the database).
####------------------------------------------------------------
crunchWHOCC <- function(codes,
                        baseurl = "https://www.whocc.no/atc_ddd_index/?code=",
                        encoding = "utf-8") {
    ## That's the vector we're using to define what to read...
    if(!missing(codes)){
        toquery <- codes
    }else{
        toquery <- c("A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R",
                     "S", "V")
    }
    message("Crunch ", baseurl, ".")
    message(" Start with ", length(toquery), " ATC codes.")
    ## That's to keep track of what we did already...
    alreadyQ <- character()
    ## That's the vector of stuff we don't want.
    excludeByName <- c("New search", "Show text", "List of")
    atcCodes <- character()
    atcNames <- character()
    ## The ddd table
    dddatc <- character()
    ddds <- numeric()
    admR <- character()
    units <- character()
    notes <- character()
    while(length(toquery) > 0){
        currentAtc <- toquery[1]
        ## Record that we did already query that one.
        alreadyQ <- c(alreadyQ, currentAtc)
        lines <- readLines(paste0(baseurl, currentAtc), encoding = encoding)
        h1 <- grep("<header>", lines)
        h2 <- grep("</header>", lines)
        if (length(h1) && length(h2))
            lines <- lines[-(h1[1L]:h2[1L])]
        doc <- htmlParse(lines, encoding = encoding)
        ## doc <- htmlParse(paste0(baseurl, currentAtc), encoding=encoding)
        ## Extract the links from the content div.
        as <- getNodeSet(doc, "//div[@id='content']//a")
        if(length(as) == 0)
            stop("Something went wrong! Did not get the expected data.")
        res <- .extractFromA(as)
        torem <- unlist(lapply(excludeByName, function(y){
            return(grep(res[, 2], pattern=y))
        }))
        if(length(torem) > 0)
            res <- res[-torem, , drop=FALSE]
        ## Eventually skip all atcCodes that have a level <= the level of the currentAtc.
        ## currentAtcLevel <- levelFromAtc(currentAtc)
        ## gotAtcLevels <- levelFromAtc(atcCodes)

        atcCodes <- c(atcCodes, res[, 1])
        atcNames <- c(atcNames, res[, 2])

        ## Parse the html table:
        theTable <- readHTMLTable(doc)
        theTable <- theTable[lengths(theTable) > 0]
        keep <- which(vapply(theTable, function(z) {
            if (is.data.frame(z) &&
                length(colnames(z)) > 0 &&
                grepl("ATC code", colnames(z)[1]))
                TRUE
            else FALSE
        }, logical(1)))
        if (length(keep) > 1)
            stop("More than one table with concentrations found for ATC ",
                 currentAtc, ".")
        if (length(keep) == 1)
            theTable <- theTable[[keep]]
        else theTable <- NULL
        if (length(theTable)) {
            colnames(theTable) <- trimws(colnames(theTable),
                                         whitespace = "[\\h\\v]")
            theTable <- as.data.frame(
                lapply(theTable, trimws, whitespace = "[\\h\\v]"),
                check.names = FALSE)
            if( !all(c("ATC code", "DDD", "U", "Adm.R", "Note") %in%
                     colnames(theTable))) {
                warning("The extracted table is not in the expected ",
                        "format for ATC: ", currentAtc)
            } else {
                theTable <- theTable[theTable[, "DDD"] != "", , drop=FALSE]
                if(nrow(theTable) > 0){
                    dddatc <- c(dddatc, as.character(theTable[, "ATC code"]))
                    ddds <- c(ddds, as.numeric(as.character(theTable[, "DDD"])))
                    admR <- c(admR, as.character(theTable[, "Adm.R"]))
                    units <- c(units, as.character(theTable[, "U"]))
                    notes <- c(notes, as.character(theTable[, "Note"]))
                }
            }
        }
        ## Next we want to add the entries.
        if(ceiling(length(alreadyQ)/100) == floor(length(alreadyQ)/100))
            message(" Already queried ", length(alreadyQ), " codes.")
        ## Determine which of the ids we should query...
        qids <- res[, 1]
        ## We don't want to query ATC codes from level 5, they should already
        ## have been returned by a query to their level 4 id.
        qids <- qids[nchar(qids) < 7]
        if(length(qids) > 0)
            toquery <- unique(c(toquery, qids))
        ## Remove all that we did already query!
        toquery <- toquery[!toquery %in% alreadyQ]
    }
    atc <- cbind(key=atcCodes, name=atcNames)
    atc <- unique(atc)
    atc <- atc[!grepl("^Hide", atc[, "name"]), , drop = FALSE]
    ddd <- data.frame(key=dddatc, ddd=ddds, unit=units, administration_route=admR,
                      note=notes, stringsAsFactors=FALSE)
    ## Add ATC codes for "" rows
    idx <- which(ddd[, 1] == "")
    while(length(idx) > 0){
        ddd[idx, 1] <- ddd[idx-1, 1]
        idx <- which(ddd[, 1] == "")
    }
    ddd <- unique(ddd)

    ## Now add the "level" column to atc:
    atc <- data.frame(atc, level=levelFromAtc(as.character(atc[, "key"])),
                      stringsAsFactors=FALSE)
    ## Define the metadata.
    met <- data.frame(name=c("Db type",
                             "Supporting package",
                             "Db created by",
                             "Creation time",
                             "sources",
                             "querycodes",
                             "DBSCHEMAVERSION"),
                      value=c("AtcDb",
                              "atc",
                              "",
                              as.character(Sys.Date()),
                              baseurl,
                              paste0(toquery, collapse=","),
                              "1.0"), stringsAsFactors=FALSE)

    return(list(atc=atc, ddd=ddd, metadata=met))
}


## x should be an XMLNodeSet
.extractFromA <- function(x){
    if(!is(x, "XMLNodeSet"))
        stop("Don't know what to do with x, should be an XMLNodeSet...")
    Values <- unlist(lapply(x, xmlValue))
    Attrs <- lapply(x, xmlAttrs)
    atcC <- unlist(lapply(Attrs, function(z){
        if(!any(names(z) == "href"))
            return(NA)
        z <- z["href"]
        ## Now split on &
        spl <- unlist(strsplit(z, split="&"))
        at <- gsub(spl[1], pattern="./?code=", replacement="", fixed=TRUE)
        return(at)
    }), use.names=FALSE)
    return(cbind(key=atcC, name=Values))
}
