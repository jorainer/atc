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
         contains="BasicFilter",
         prototype=list(
             condition="=",
             value="",
             .valueIsCharacter=TRUE
         ))
KeyFilter <- function(value, condition="="){
    if(missing(value))
        stop("Missing argument 'value'.")
    if(length(value) > 1){
        if(condition != "in" | condition != "not in"){
            if(condition == "="){
                condition <- "in"
            }else{
                if(condition == "!="){
                    condition <- "not in"
                }else{
                    stop("For length of 'value' > 1, condition has to be '=', '!=',",
                         " 'in' or 'not in'.")
                }
            }
        }
    }
    return(new("KeyFilter", condition=condition, value=as.character(value)))
}

####============================================================
##  NameFilter
##
setClass("NameFilter",
         contains="BasicFilter",
         prototype=list(
             condition="=",
             value="",
             .valueIsCharacter=TRUE
         ))
NameFilter <- function(value, condition="="){
    if(missing(value))
        stop("Missing argument 'value'.")
    if(length(value) > 1){
        if(condition != "in" | condition != "not in"){
            if(condition == "="){
                condition <- "in"
            }else{
                if(condition == "!="){
                    condition <- "not in"
                }else{
                    stop("For length of 'value' > 1, condition has to be '=', '!=',",
                         " 'in' or 'not in'.")
                }
            }
        }
    }
    return(new("NameFilter", condition=condition, value=as.character(value)))
}

####============================================================
##  LevelFilter
##
setClass("LevelFilter",
         contains="BasicFilter",
         prototype=list(
             condition="=",
             value="",
             .valueIsCharacter=FALSE
         ))
LevelFilter <- function(value, condition="="){
    if(missing(value))
        stop("Missing argument 'value'.")
    if(!is.numeric(value))
        stop("'value' has to be numeric!")
    if(length(value) > 1){
        if((condition != "in") | (condition != "not in")){
            if(condition == "="){
                condition <- "in"
            }else{
                if(condition == "!="){
                    condition <- "not in"
                }else{
                    stop("For length of 'value' > 1, condition has to be '=', '!=',",
                         " 'in' or 'not in'.")
                }
            }
        }
    }
    return(new("LevelFilter", condition=condition, value=as.character(value)))
}

####============================================================
##  AdminrouteFilter
##
setClass("AdminrouteFilter",
         contains="BasicFilter",
         prototype=list(
             condition="=",
             value="",
             .valueIsCharacter=TRUE
         ))
AdminrouteFilter <- function(value, condition="="){
    if(missing(value))
        stop("Missing argument 'value'.")
    if(length(value) > 1){
        if((condition != "in") | (condition != "not in")){
            if(condition == "="){
                condition <- "in"
            }else{
                if(condition == "!="){
                    condition <- "not in"
                }else{
                    stop("For length of 'value' > 1, condition has to be '=', '!=',",
                         " 'in' or 'not in'.")
                }
            }
        }
    }
    return(new("AdminrouteFilter", condition=condition, value=as.character(value)))
}
