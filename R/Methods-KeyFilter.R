## ####============================================================
## ##  Methods for the KeyFilter class.

####============================================================
##  column
setMethod("column",
          signature(object="KeyFilter", db="missing", with.tables="missing"),
          function(object, db, with.tables, ...){
              return("key")
          })
setMethod("column",
          signature(object="KeyFilter", db="AtcDb",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              tn <- names(listTables(db))
              return(column(object, db, with.tables=tn))
          })
setMethod("column",
          signature(object="KeyFilter", db="AtcDb",
                    with.tables="character"),
          function(object, db, with.tables, ...){
              return(unlist(prefixColumns(db, "key", with.tables=with.tables),
                            use.names=FALSE))
          })

####============================================================
##  where
setMethod("where",
          signature(object="KeyFilter", db="missing",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              ## without a database we're just calling the where of BasicFilter
              .where(object)
          }
          )
setMethod("where",
          signature(object="KeyFilter", db="AtcDb",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              tn <- names(listTables(db))
              return(where(object, db, with.tables=tn))
          })
setMethod("where",
          signature(object="KeyFilter", db="AtcDb",
                    with.tables="character"),
          function(object, db, with.tables, ...){
              suff <- where(object) ## that returns the result from the BasicFilter...
              return(paste(column(object, db, with.tables), suff))
          })

