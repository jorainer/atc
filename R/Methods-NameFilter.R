####============================================================
##  Methods for the NameFilter class.

####============================================================
##  column
setMethod("column",
          signature(object="NameFilter", db="missing", with.tables="missing"),
          function(object, db, with.tables, ...){
              return("name")
          })
setMethod("column",
          signature(object="NameFilter", db="AtcDb",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              tn <- names(listTables(db))
              return(column(object, db, with.tables=tn))
          })
setMethod("column",
          signature(object="NameFilter", db="AtcDb",
                    with.tables="character"),
          function(object, db, with.tables, ...){
              return(unlist(prefixColumns(db, "name", with.tables=with.tables),
                            use.names=FALSE))
          })

####============================================================
##  where
setMethod("where",
          signature(object="NameFilter", db="missing",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              ## without a database we're just calling the where of BasicFilter
              .where(object)
          }
          )
setMethod("where",
          signature(object="NameFilter", db="AtcDb",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              tn <- names(listTables(db))
              return(where(object, db, with.tables=tn))
          })
setMethod("where",
          signature(object="NameFilter", db="AtcDb",
                    with.tables="character"),
          function(object, db, with.tables, ...){
              suff <- where(object) ## that returns the result from the BasicFilter...
              return(paste(column(object, db, with.tables), suff))
          })

