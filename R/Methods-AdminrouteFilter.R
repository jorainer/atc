####============================================================
##  Methods for the AdminrouteFilter class.

####============================================================
##  column
setMethod("column",
          signature(object="AdminrouteFilter", db="missing", with.tables="missing"),
          function(object, db, with.tables, ...){
              return(NULL)
          })
setMethod("column",
          signature(object="AdminrouteFilter", db="AtcDb",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              tn <- names(listTables(db))
              return(column(object, db, with.tables=tn))
          })
setMethod("column",
          signature(object="AdminrouteFilter", db="AtcDb",
                    with.tables="character"),
          function(object, db, with.tables, ...){
              return(unlist(prefixColumns(db, "administration_route", with.tables=with.tables),
                            use.names=FALSE))
          })

####============================================================
##  where
setMethod("where",
          signature(object="AdminrouteFilter", db="missing",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              ## without a database we're just calling the where of BasicFilter
              return(callNextMethod())
          }
          )
setMethod("where",
          signature(object="AdminrouteFilter", db="AtcDb",
                    with.tables="missing"),
          function(object, db, with.tables, ...){
              tn <- names(listTables(db))
              return(where(object, db, with.tables=tn))
          })
setMethod("where",
          signature(object="AdminrouteFilter", db="AtcDb",
                    with.tables="character"),
          function(object, db, with.tables, ...){
              suff <- where(object) ## that returns the result from the BasicFilter...
              return(paste(column(object, db, with.tables), suff))
          })

