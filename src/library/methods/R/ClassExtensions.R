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
.simpleExtTest <- function(object)TRUE
## TO DO:  the simple replace below only handles the case of classes with slots.
## There are some other simple relations (e.g., with virtual classes).  Replacing in
## these cases is less likely but needs to be tested (below) and a suitable
## replace function inserted.
.simpleExtReplace <- function(object, Class, value){
    for(what in slotNames(Class))
        slot(object, what) <- slot(value, what)
    object
}
.dataPartReplace <- function(object, Class, value){
    object@.Data <- value
    object
}

.dataPartReplace2 <- function(object, Class, value){
    object@.Data <- as(value, THISCLASS, strict = FALSE)
    object
}
.ErrorReplace <- function(object, Class, value)
    stop(paste("No replace method was defined for as(x, \"", Class,
               "\") <- value for class \"", class(object), "\"", sep=""))

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
    simple <- is.null(coerce) && is.null(test) && is.null(replace) && (length(by)==0)
    dataPartClass <- elNamed(slots, ".Data")
    dataPart <- simple && !is.null(dataPartClass) && extends(to, dataPartClass)
    if(is.null(coerce)) {
        coerce <- .simpleExtCoerce
        if(!isVirtualClass(classDef2))
            body(coerce, envir = environment(coerce)) <-
                 .simpleCoerceExpr(Class, to, slots)
    }
    if(is.null(test))
        test <- .simpleExtTest
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
        else if(simple)
            replace <- .simpleExtReplace
        else
            replace <- .ErrorReplace
    }
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
    
