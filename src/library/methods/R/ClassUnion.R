#  File src/library/methods/R/ClassUnion.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

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
    setClassUnion("OptionalFunction", c("function", "NULL"), where)
    setClassUnion("PossibleMethod", c("function", "MethodDefinition"), where)
    clList <- c("ClassUnionRepresentation", "OptionalFunction",
                "PossibleMethod")
    assign(".SealedClasses", c(get(".SealedClasses", where), clList), where)
}

setClassUnion <- function(name, members = character(), where = topenv(parent.frame())) {
    if(length(members)>0) {
        membersDefined <- sapply(members, isClass, where = as.environment(where))
        if(!all(membersDefined))
            stop(gettextf("the member classes must be defined: not true of %s",
                          paste(.dQ(as(members[!membersDefined], "character")), collapse=", ")), domain = NA)
    }
    def <- new("ClassUnionRepresentation",
               makeClassRepresentation(name, package = getPackageName(where), where = where))
    prev <- getClassDef(name, where = where)
    value <- setClass(name, def, where = where)
    failed <- character()
    ## the prototype of the union will be from the first non-virtual
    ## subclass, except that we prefer NULL if "NULL" is a subclass
    hasNull <- match("NULL", members, 0)
    if(hasNull)
        members <- c("NULL", members[-hasNull])
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
                      paste(.dQ(failed), collapse=", ")), domain = NA)
    }
    invisible(value)
}

isClassUnion <- function(Class) {
    ## test the class DEFINITION for representing a union
    if(is.character(Class))
        Class <- getClass(Class, TRUE) # the real def. or a dummy
    extends(class(Class), "ClassUnionRepresentation")
}
