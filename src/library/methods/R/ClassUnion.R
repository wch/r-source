.InitClassUnion <- function(where) {
    setClass("ClassUnionRepresentation",  "classRepresentation",
             validity =function(object) {
                 if(identical(object@virtual, TRUE) && length(object@slots)==0 &&
                    is.null(object@prototype))
                     TRUE
                 else
                     "Class must be an empty virtual class with NULL prototype"
             }, where = where)
    ## some classes in methods package are unions--now they can be officially
    setClassUnion("OptionalFunction", c("function", "NULL"))
    setClassUnion("PossibleMethod", c("function", "MethodDefinition"))
}

setClassUnion <- function(name, members = character(), where = topenv(parent.frame())) {
    if(length(members)>0) {
        membersDefined <- sapply(members, isClass, where = as.environment(where))
        if(!all(membersDefined))
            stop(gettextf("the member classes must be defined: not true of %s",
                          paste(dQuote(as(members, "character")), collapse=", ")), domain = NA)
    }
    def <- new("ClassUnionRepresentation",
               makeClassRepresentation(name, package = getPackageName(where), where = where))
    prev <- getClassDef(name, where = where)
    value <- setClass(name, def, where = where)
    failed <- character()
    for(what in members) {
        if(is(try(setIs(what, name, where = where)), "try-error")) {
            if(!is.character(what))
                what <- getClass(what, TRUE, where)@className
            failed <- c(failed, what)
        }
    }
    if(length(failed)>0) {
        if(is.null(prev))
            try(removeClass(name, where = where))
        else
            try(setClass(name, prev, where = where))
        stop(gettextf("unable to create union class:  could not set members %s",
                      paste(dQuote(failed), collapse=", ")), domain = NA)
    }
    value
}

isClassUnion <- function(Class) {
    ## test the class DEFINITION for representing a union
    if(is.character(Class))
        Class <- getClass(Class, TRUE) # the real def. or a dummy
    extends(class(Class), "ClassUnionRepresentation")
}
