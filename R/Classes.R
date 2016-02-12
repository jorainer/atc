####============================================================
##  AtcDb
##
##  The main AtcDb class that provides the connection to the
##  database
####------------------------------------------------------------
setClass("AtcDb",
         representation(atcdb="DBIConnection", .properties="list", tables="list"),
         prototype=list(atcdb=NULL, .properties=list(), tables=list())
         )


