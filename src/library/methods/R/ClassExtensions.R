.InitExtensions <- function(where) {
    ## to be called from the initialization
    setClass("SClassExtension", representation(subClass = "character", superClass = "character", package = "character",
                                               coerce = "function", test = "function",
                                               replace = "function",
                                               simple = "logical", by = "character",
                                               dataPart = "logical"),
             where = where)
    assign(".SealedClasses", c(get(".SealedClasses", where), "SClassExtension"), where);
}

.simpleExtCoerce <- function(from, strict = TRUE)from
.simpleIsCoerce <- function(from)from
.simpleExtTest <- function(object)TRUE
## TO DO:  the simple replace below only handles the case of classes with slots.
## There are some other simple relations (e.g., with virtual classes).  Replacing in
## these cases is less likely but needs to be tested (below) and a suitable
## replace function inserted.
.simpleExtReplace <- function(from, to, value){
    for(what in .InhSlotNames(to))
        slot(from, what) <- slot(value, what)
    from
}
## slot names for inheritance (to be used in replace methods).  Extends slots to implicit
## .Data for basic classes.
.InhSlotNames <- function(Class) {
   ClassDef <- getClass(Class)
    value <- names(ClassDef@slots)
    if(length(value)==0 && (Class %in% .BasicClasses || extends(ClassDef, "vector")))
        ## No slots, but extends "vector" => usually a basic class; treat as data part
        value <- ".Data"
   value
}
.dataPartReplace <- function(from, to, value){
    from@.Data <- value
    from
}

.dataPartReplace2 <- function(from, to, value){
    from@.Data <- as(value, THISCLASS, strict = FALSE)
    from
}

## and a version of dataPartReplace w/o the unused `to' argument
.dataPartReplace2args <- function(from, value) {
    from@.Data <- value
    from
}
    
.ErrorReplace <- function(from, to, value)
    stop(paste("No replace method was defined for as(x, \"", to,
               "\") <- value for class \"", class(from), "\"", sep=""))

.objectSlotNames <- function(object) {
    ## a quick version that makes no attempt to check the class definition
    value <- names(attributes(object))
    if(is.null(value)) ## not possible with methods package?
        character()
    else
        value[-match("class", value, 0)]
}

makeExtends <- function(Class, to,
                        coerce = NULL, test = NULL, replace = NULL,
                        by = character(), package,
                        slots = getSlots(classDef1),
                        classDef1 = getClass(Class), classDef2) {
    ## test for datapart class:  must be the data part class, except
    ## that extensions within the basic classes are allowed (numeric, integer)
    dataEquiv <- function(cl1, cl2) {
        .identC(cl1, cl2) ||
          (extends(cl1, cl2) && !any(is.na(match(c(cl1, cl2), .BasicClasses))))
    }
    packageEnv <- .requirePackage(package)
    class1Defined <- missing(slots) # only at this time can we construct methods
    simple <- is.null(coerce) && is.null(test) && is.null(replace) && (length(by)==0)
    dataPartClass <- elNamed(slots, ".Data")
    dataPart <- FALSE
    if(simple && !is.null(dataPartClass)) {
        if(!(is.null(getClassDef(dataPartClass)) || is.null(getClassDef(to)))) {
            ## note that dataPart, to are looked up in the methods package & parents,
            ## because the default in getClassDef is the topenv of the caller (this fun.):
            ## Assertion is that only these classes are allowed as data slots
            dataPart <- dataEquiv(dataPartClass, to)
        }
    }
    if(is.null(coerce)) {
        coerce <- .simpleExtCoerce
        if(!isVirtualClass(classDef2))
            body(coerce, envir = packageEnv) <-
                 .simpleCoerceExpr(Class, to, names(slots), classDef2)
    }
    else if(is(coerce, "function")) {
        ## we allow definitions with and without the `strict' argument
        ## but create a  function that can be called with the argument
        if(length(formals(coerce)) == 1) {
            coerce <- .ChangeFormals(coerce, .simpleIsCoerce, "`coerce' argument to setIs ")
            tmp <- .simpleExtCoerce
            body(tmp, envir = environment(coerce)) <- body(coerce)
            coerce <- tmp
        }
        else
            coerce <- .ChangeFormals(coerce, .simpleExtCoerce, "`coerce' argument to setIs ")
        
    }
    else stop("The `coerce' argument to setIs should be a function of one argument, got an object of class \"",
              class(coerce), "\"")
    if(is.null(test))
        test <- .simpleExtTest
    else
        test <- .ChangeFormals(test, .simpleExtTest, "`test' argument to setIs ")
    if(is.null(replace)) {
        if(dataPart) {
            extn <- elNamed(classDef2@contains, dataPartClass)
            if(is(extn, "SClassExtension"))
                easy <- extn@simple
            else
                easy <- FALSE
            if(easy)
                replace <- .dataPartReplace
            else {
                replace <- .dataPartReplace2
                bdy <- body(replace)
                body(replace, envir = environment(replace)) <-
                    substituteDirect(bdy, list(THISCLASS = dataPartClass))
            }
        }
        else if(simple) {
            replace <- .simpleExtReplace
            if(isVirtualClass(classDef2)) {  # a simple is to a virtual class => a union
                body(replace, envir = packageEnv) <-
                    substitute({
                        if(!is(value, TO))
                            stop("The computation: as(object,\"", TO,
                                 "\") <- value is valid when object has class",
                                 FROM, "\" only if is(value, \"",TO,"\") is TRUE ( class(value) was \"",
                                 class(value), "\")")
                        value
                    }, list(FROM = Class, TO = to))
            }
            else if(class1Defined && length(slots) == 0) {
                ## check for the classes having the same representation
                ## (including the case of no slots)
                ext <- getAllSuperClasses(classDef1, TRUE)
                toSlots <- classDef2@slots
                sameSlots <- TRUE
                for(eclass in ext) {
                    ## does any superclass other than "to" have slots?
                    if(.identC(eclass, to))
                        next
                    edef <- getClassDef(eclass, where = packageEnv)
                    if(!is.null(edef) && length(edef@slots) > 0) {
                        sameSlots <- FALSE
                        break
                    }
                }
                if(sameSlots)
                    body(replace, envir = packageEnv) <-
                        substitute({class(value) <- FROM; value}, list(FROM = Class))
                else if(length(toSlots) == 0) # seems replacement not defined in this case?
                    replace <- .ErrorReplace
            }
            else
                body(replace, envir = packageEnv) <-
                    .simpleReplaceExpr(classDef2)
        }
        else
            replace <- .ErrorReplace
        if(identical(replace, .ErrorReplace))
            warning("There is no automatic definition for as(object, \"", to,
                    "\") <- value when object has class \"", Class,
                    "\" and no replace= argument was supplied; replacement will be an error")
    }
    else if(is(replace, "function")) {
        ## turn function of two or three arguments into correct 3-arg form
        if(length(formals(replace)) == 2) {
            replace <- .ChangeFormals(replace, .dataPartReplace2args, "`replace' argument to setIs ")
            tmp  <- .ErrorReplace
            body(tmp, envir = environment(replace)) <- body(replace)
            replace <- tmp
        }
        else
            replace <- .ChangeFormals(replace, .ErrorReplace, "`replace' argument to setIs ")
    }
    else
        stop("the replace= argument to setIs() should be a function of 2 or 3 arguments, got an object of class \"",
             class(replace), "\"")
    new("SClassExtension", subClass = Class, superClass = to, package = package, coerce = coerce,
               test = test, replace = replace, simple = simple, by = by, dataPart = dataPart)
    
    
}

.findAll <- function(what, where = topenv(parent.frame())) {
    ## search in envir. & parents thereof
    ## must avoid R's find() function because it uses
    ## regular expressions
    value <- list()
    if(is.environment(where))
        repeat {
            if(exists(what, where, inherits = FALSE))
                value <- c(value, list(where))
            ## two forms of test for the end of the parent env. chain
            if(isBaseNamespace(where) || is.null(where))
                break
            where <- parent.env(where)
        }
    else
        for(i in where) {
            if(exists(what, i, inherits = FALSE))
                value <- c(value, list(i))
        }
    value
}
    
