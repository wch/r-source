.InitExtensions <- function(where) {
    ## to be called from the initialization
    setClass("SClassExtension", representation(superClass = "character", package = "character",
                                               coerce = "function", test = "function",
                                               replace = "function",
                                               simple = "logical", by = "character",
                                               dataPart = "logical"),
             sealed = TRUE, where = where)
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
    if(length(value)==0 && !is.na(match("vector", names(ClassDef@contains))))
        ## usually a basic class; treat as data part
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
                        by = character(), package = getPackageName(findClass(to)),
                        slots = getSlots(classDef1),
                        classDef1 = getClass(Class, TRUE), classDef2 = getClass(to, TRUE)) {
    ## test for datapart class:  must be the data part class, except
    ## that extensions within the basic classes are allowed (numeric, integer)
    dataEquiv <- function(cl1, cl2) {
        identical(cl1, cl2) ||
          (extends(cl1, cl2) && !any(is.na(match(c(cl1, cl2), .BasicClasses))))
    }
    class1Defined <- missing(slots) # only at this time can we construct methods
    simple <- is.null(coerce) && is.null(test) && is.null(replace) && (length(by)==0)
    dataPartClass <- elNamed(slots, ".Data")
    dataPart <- simple && !is.null(dataPartClass) && dataEquiv(to, dataPartClass)
    if(is.null(coerce)) {
        coerce <- .simpleExtCoerce
        if(!isVirtualClass(classDef2))
            body(coerce, envir = environment(coerce)) <-
                 .simpleCoerceExpr(Class, to, slots)
    }
    else if(is(coerce, "function")) {
        ## we allow definitions with and without the `strict' argument
        if(length(formals(coerce)) > 1)
            forArgs <- .simpleExtCoerce
        else
            forArgs <- .simpleIsCoerce
        coerce <- .ChangeFormals(coerce, forArgs, "`coerce' argument to setIs ")
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
            if(isVirtualClass(to)) {  # a simple is to a virtual class => a union
                body(replace, envir = environment(replace)) <-
                    substitute({
                        if(!is(value, FROM))
                            stop("The computation: as(object,\"", TO,
                                 "\") <- value is valid when object has class",
                                 FROM, "\" only if is(value, \"",TO,"\") is TRUE ( class(value) was \"",
                                 class(value), "\")")
                        value
                    }, list(FROM = Class, TO = to))
            }
            else if(class1Defined && length(slots) == 0) {
                ## check for Class, to having the same representation
                ## (including the case of no slots)
                ext <- getAllSuperClasses(classDef1)
                toSlots <- classDef2@slots
                sameSlots <- TRUE
                for(eclass in ext)
                    if(!identical(eclass, to) && isClass(eclass) &&
                       length(getClassDef(eclass)@slots) > 0) {
                        sameSlots <- FALSE
                        break
                    }
                if(sameSlots)
                    body(replace, envir = environment(replace)) <-
                        substitute({class(value) <- FROM; value}, list(FROM = Class))
                else if(length(toSlots) == 0) # seems replacement not defined in this case?
                    replace <- .ErrorReplace
            }
        }
        else
            replace <- .ErrorReplace
        if(identical(replace, .ErrorReplace))
            warning("There is no automatic definition for as(object, \"", to,
                    "\") <- value when object has class \"", Class,
                    "\" and no replace= argument was supplied; replacement will be an error")
    }
    else
        replace <- .ChangeFormals(replace, .ErrorReplace, "`replace' argument to setIs ")
    new("SClassExtension", superClass = to, package = package, coerce = coerce,
               test = test, replace = replace, simple = simple, by = by, dataPart = dataPart)
    
    
}

.findMetaData <- function(what, where = search()) {
    ## must avoid R's find() function because it uses
    ## regular expressions
    ok <- logical(length(where))
    for(i in seq(along=where))
        ok[i] <- exists(what, where[i], inherits = FALSE)
    where[ok]
}
    
