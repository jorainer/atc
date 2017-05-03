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


####============================================================
##  KeyFilter
##
setClass("KeyFilter",
         contains="CharacterFilter",
         prototype=list(
             condition="==",
             value="",
             field = "key"
         ))
KeyFilter <- function(value, condition="=="){
    if(missing(value))
        stop("Missing argument 'value'.")
    ## if(length(value) > 1){
    ##     if(condition != "in" | condition != "not in"){
    ##         if(condition == "=="){
    ##             condition <- "in"
    ##         }else{
    ##             if(condition == "!="){
    ##                 condition <- "not in"
    ##             }else{
    ##                 stop("For length of 'value' > 1, condition has to be '==', '!=',",
    ##                      " 'in' or 'not in'.")
    ##             }
    ##         }
    ##     }
    ## }
    ## return(new("KeyFilter", condition = condition, value = as.character(value)))
    res <- new("KeyFilter", condition = condition, value = as.character(value))
    if (validObject(res))
        res
}

####============================================================
##  NameFilter
##
setClass("NameFilter",
         contains="CharacterFilter",
         prototype=list(
             condition = "==",
             value = "",
             field = "name"
         ))
NameFilter <- function(value, condition = "=="){
    if(missing(value))
        stop("Missing argument 'value'.")
    ## if(length(value) > 1){
    ##     if(condition != "in" | condition != "not in"){
    ##         if(condition == "=="){
    ##             condition <- "in"
    ##         }else{
    ##             if(condition == "!="){
    ##                 condition <- "not in"
    ##             }else{
    ##                 stop("For length of 'value' > 1, condition has to be '==', '!=',",
    ##                      " 'in' or 'not in'.")
    ##             }
    ##         }
    ##     }
    ## }
    ## return(new("NameFilter", condition=condition, value=as.character(value)))
    res <- new("NameFilter", condition = condition, value = as.character(value))
    if (validObject(res))
        res
}

####============================================================
##  LevelFilter
##
setClass("LevelFilter",
         contains="IntegerFilter",
         prototype=list(
             condition = "==",
             value = integer(),
             field = "level"
         ))
LevelFilter <- function(value, condition = "=="){
    if(missing(value))
        stop("Missing argument 'value'.")
    ## if(!is.numeric(value))
    ##     stop("'value' has to be numeric!")
    ## if(length(value) > 1){
    ##     if((condition != "in") | (condition != "not in")){
    ##         if(condition == "=="){
    ##             condition <- "in"
    ##         }else{
    ##             if(condition == "!="){
    ##                 condition <- "not in"
    ##             }else{
    ##                 stop("For length of 'value' > 1, condition has to be '==', '!=',",
    ##                      " 'in' or 'not in'.")
    ##             }
    ##         }
    ##     }
    ## }
    ## return(new("LevelFilter", condition = condition, value=as.integer(value)))
    res <- new("LevelFilter", condition = condition, value = as.integer(value))
    if (validObject(res))
        res
}

####============================================================
##  AdminrouteFilter
##
setClass("AdminrouteFilter",
         contains="CharacterFilter",
         prototype=list(
             condition = "==",
             value = "",
             field = "adminroute"
         ))
AdminrouteFilter <- function(value, condition = "=="){
    if(missing(value))
        stop("Missing argument 'value'.")
    ## if(length(value) > 1){
    ##     if((condition != "in") | (condition != "not in")){
    ##         if(condition == "=="){
    ##             condition <- "in"
    ##         }else{
    ##             if(condition == "!="){
    ##                 condition <- "not in"
    ##             }else{
    ##                 stop("For length of 'value' > 1, condition has to be '==', '!=',",
    ##                      " 'in' or 'not in'.")
    ##             }
    ##         }
    ##     }
    ## }
    ## return(new("AdminrouteFilter", condition = condition,
    ##            value = as.character(value)))
    res <- new("AdminrouteFilter", condition = condition,
               value = as.character(value))
    if (validObject(res))
        res
}
