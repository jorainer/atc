####============================================================
##  Methods for the LevelFilter class.

####============================================================
##  column
setMethod("column",
          signature(object="LevelFilter", db="missing", with.tables="missing"),
          function(object, db, with.tables, ...){
              return("level")
          })
setMethod("column",
          signature(object="LevelFilter", db="AtcDb",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              tn <- names(listTables(db))
              return(column(object, db, with.tables=tn))
          })
setMethod("column",
          signature(object="LevelFilter", db="AtcDb",
                    with.tables="character"),
          function(object, db, with.tables, ...){
              return(unlist(prefixColumns(db, "level", with.tables=with.tables),
                            use.names=FALSE))
          })

####============================================================
##  where
setMethod("where",
          signature(object="LevelFilter", db="missing",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              ## without a database we're just calling the where of BasicFilter
              return(callNextMethod())
          }
          )
setMethod("where",
          signature(object="LevelFilter", db="AtcDb",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              tn <- names(listTables(db))
              return(where(object, db, with.tables=tn))
          })
setMethod("where",
          signature(object="LevelFilter", db="AtcDb",
                    with.tables="character"),
          function(object, db, with.tables, ...){
              suff <- where(object) ## that returns the result from the BasicFilter...
              return(paste(column(object, db, with.tables), suff))
          })

