## Just some simple code that was used to generate the ATC data.frame
## combining the information from http://www.wido.de (ATC codes for 2016) for
## german category and medication names and from http://atccode.com for the
## english category names.
wido <- read.table("./ATC_categories_wido_2016.txt", sep="\t", as.is=TRUE, header=TRUE)

## Process the atccodes.com data
ats <- readLines("./ATC_categories_raw_atccode_com.txt")
ats <- ats[-1]
ats <- ats[ats!=""]
ats <- strsplit(ats, split=" : ", fixed=TRUE)
ats <- do.call(rbind, ats)
ats[, 1] <- gsub(ats[, 1], pattern=" ", replacement="", fixed=TRUE)

rownames(ats) <- ats[, 1]
rownames(wido) <- wido[, 1]

## Joining the two.
wido <- cbind(wido, name_en=rep(NA, nrow(wido)))
wido[rownames(ats), "name_en"] <- ats[, 2]

NAs <- is.na(wido[, "name_en"])
wido[NAs, ]

write.table(wido, file="atcCategories.txt", sep="\t", row.names=FALSE)

## Do some remaining checks and edits in the file.
atc <- read.table("atcCategories.txt", sep="\t", as.is=TRUE, header=TRUE)
head(atc)
atc <- cbind(atc, level=rep(NA, nrow(atc)))

ncars <- nchar(atc$key)
atc[ncars==1, "level"] <- 1
atc[ncars==3, "level"] <- 2
atc[ncars==4, "level"] <- 3
atc[ncars==5, "level"] <- 4

## read and include also the 5th level...
fifth <- read.table("./atcFifthLevel.txt", sep="\t", as.is=TRUE, header=TRUE)
fifth <- cbind(fifth, level=rep(5, nrow(fifth)), name_en=rep(NA, nrow(fifth)))
fifth <- fifth[, colnames(atc)]
rownames(fifth) <- fifth$key
## OK, now we're done.
write.table(atc, file="atcLevel1to4.txt", sep="\t", row.names=FALSE)
save(atc, file="atcLevel1to4.RData", compress="xz")
write.table(fifth, file="atcLevel5.txt", sep="\t", row.names=FALSE)
save(fifth, file="atcLevel5.RData", compress="xz")


####============================================================
##  Generating an SQLite database.
##
##load("../data/atcLevel5.RData")
##load("../data/atcLevel1to4.RData")
atc <- read.table("atcLevel1to4.txt", sep="\t", as.is=TRUE, header=TRUE)
fifth <- read.table("./atcFifthLevel.txt", sep="\t", as.is=TRUE, header=TRUE)

ddd <- fifth[, c("key", "ddd")]
ddd <- ddd[ddd$ddd!="", ]

fifth <- cbind(fifth, name_en=rep(NA, nrow(fifth)), level=rep(5, nrow(fifth)))

fullatc <- rbind(atc, fifth[, colnames(atc)])

library(RSQLite)
con <- dbConnect(dbDriver("SQLite"), dbname="atc.sqlite")
dbWriteTable(con, name="atc", fullatc, row.names=FALSE)
dbWriteTable(con, name="ddd", ddd, row.names=FALSE)
dbGetQuery(con, "create index key_idx on atc (key);")
dbGetQuery(con, "create index level_idx on atc (level);")
dbGetQuery(con, "create index ddd_key_idx on ddd (key);")
met <- data.frame(name=c("Db type", "Supporting package", "Db created by",
                         "Creation time", "sources", "DBSCHEMAVERSION"),
                  value=c("AtcDb", "atc", "johannes.rainer@eurac.edu", "2016-02-10",
                          "http://www.wido.de, http://atccodes.com, https://en.wikipedia.org",
                          "1.0"), stringsAsFactors=FALSE)
dbWriteTable(con, name="metadata", met, row.names=FALSE)
dbDisconnect(con)

