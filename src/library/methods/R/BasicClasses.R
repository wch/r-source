.InitBasicClasses <-
  function(envir)
{
    ## setClass won't allow redefining basic classes,
    ## so make the list of these empty for now.
    assign(".BasicClasses", character(), envir)
    ## hide some functions that would break because the basic
    ## classes are not yet defined
    real.reconcileP <- reconcilePropertiesAndPrototype
    assign("reconcilePropertiesAndPrototype", function(name, properties, prototype, extends) {
        list(properties=properties, prototype = prototype, extends = extends)
    }, envir)
    setClass("VIRTUAL", sealed = TRUE, where = envir)
    setClass("oldClass", sealed = TRUE, where = envir)
    setClass("ANY", sealed = TRUE, where = envir)
    setClass("vector", sealed = TRUE, where = envir)
    setClass("missing", sealed = TRUE, where = envir)
    vClasses <- c("logical", "numeric", "character",
                "complex", "integer", "single", "double",
                "expression", "list")
    for(.class in vClasses) {
        setClass(.class, prototype = newBasic(.class), sealed = TRUE, where = envir)
    }
    clList <- c(vClasses, "VIRTUAL", "ANY", "vector", "missing")
    nullF <- function()NULL; environment(nullF) <- .GlobalEnv
    setClass("function", prototype = nullF, sealed = TRUE, where = envir); clList <- c(clList, "function")

    setClass("language", sealed = TRUE, where = envir); clList <- c(clList, "language")
    setClass("environment", prototype = new.env(), sealed = TRUE, where = envir); clList <- c(clList, "environment")

    setClass("externalptr", prototype = .newExternalptr(), sealed = TRUE, where = envir); clList <- c(clList, "externalptr")
             

    ## NULL is weird in that it has NULL as a prototype, but is not virtual
    nullClass <- newClassRepresentation(className="NULL", prototype = NULL, virtual=FALSE)
    assignClassDef("NULL", nullClass, where = envir); clList <- c(clList, "NULL")

    
    setClass("structure", sealed = TRUE, where = envir); clList <- c(clList, "structure")
    stClasses <- c("matrix", "array", "ts")
    for(.class in stClasses) {
        setClass(.class, prototype = newBasic(.class), sealed = TRUE, where = envir)
    }
    clList <- c(clList, stClasses)
    assign(".BasicClasses", clList, envir)

    ## Now we can define the SClassExtension class and use it to instantiate some
    ## is() relations.
    .InitExtensions(envir)

    for(.class in vClasses)
        setIs(.class, "vector", where = envir)
    
    setIs("double", "numeric", where = envir)
    setIs("integer", "numeric", where = envir)

    setIs("structure", "vector", coerce = function(object) as.vector(object), where = envir)
    
    for(.class in stClasses)
        setIs(.class, "structure", where = envir)
    setIs("matrix", "array", where = envir)
    setIs("array", "matrix", test = function(object) length(dim(object)) == 2, where = envir)

    ## Some class definitions extending "language", delayed to here so
    ## setIs will work.
    setClass("name", "language", prototype = as.name("<UNDEFINED>"), sealed = TRUE, where = envir); clList <- c(clList, "name")
    setClass("call", "language", prototype = quote("<undef>"()), sealed = TRUE, where = envir); clList <- c(clList, "call")
    setClass("{", "language", prototype = quote({}), sealed = TRUE, where = envir); clList <- c(clList, "{")
    setClass("if", "language", prototype = quote(if(NA) TRUE else FALSE), sealed = TRUE, where = envir); clList <- c(clList, "if")
    setClass("<-", "language", prototype = quote("<undef>"<-NULL), sealed = TRUE, where = envir); clList <- c(clList, "<-")
    setClass("for", "language", prototype = quote(for(NAME in logical()) NULL), sealed = TRUE, where = envir); clList <- c(clList, "for") 
    setClass("while", "language", prototype = quote(while(FALSE) NULL), sealed = TRUE, where = envir); clList <- c(clList, "while") 
    setClass("repeat", "language", prototype = quote(repeat{break}), sealed = TRUE, where = envir); clList <- c(clList, "repeat") 
    setClass("(", "language", prototype = quote((NULL)), sealed = TRUE, where = envir); clList <- c(clList, "(") 

    ## a virtual class used to allow NULL as an indicator that a possible function
    ## is not supplied (used, e.g., for the validity slot in classRepresentation
    setClass("OptionalFunction", sealed = TRUE, where = envir)
    setIs("function", "OptionalFunction", where = envir)
    setIs("NULL", "OptionalFunction")
    assign(".BasicClasses", clList, envir)
    ## call setOldClass on some known old-style classes.  Ideally this would be done
    ## in the code that uses the classes, but that code doesn't know about the methods
    ## package.
    for(cl in .OldClassesList)
        setOldClass(cl, envir)
    ## restore the true definition of the hidden functions
    assign("reconcilePropertiesAndPrototype", real.reconcileP, envir)
}

### The following methods are not currently installed.  (Tradeoff between intuition
### of users that new("matrix", ...) should be like matrix(...) vs
### consistency of new().  Relevant when new class has basic class as its data part.
###
### To install the methods below, uncomment the last line of the function
### .InitMethodDefinitions
.InitBasicClassMethods <- function(where) {
    ## methods to initialize "informal" classes by using the
    ## functions of the same name.
    ##
    ## These methods are designed to be inherited or extended
    setMethod("initialize", "matrix",
              function(object, data =   NA, nrow = 1, ncol = 1,
                       byrow = FALSE, dimnames = NULL, ...) {
                  if(nargs() < 2) # guaranteed to be called with object from new
                      object
                  else if(is.matrix(data) && nargs() == 2 + length(list(...)))
                      .mergeAttrs(data, object, list(...))
                  else {
                      value <- matrix(data, nrow, ncol, byrow, dimnames)
                      .mergeAttrs(value, object, list(...))
                  }
              })
    setMethod("initialize", "array",
              function(object, data =   NA, dim = length(data), dimnames = NULL, ...) {
                  if(nargs() < 2) # guaranteed to be called with object from new
                      object
                  else if(is.array(data) && nargs() == 2 + length(list(...)))
                      .mergeAttrs(data, object, list(...))
                  else {
                      value <- array(data, nrow, ncol, byrow, dimnames)
                      .mergeAttrs(value, object, list(...))
                  }
              })
    ## the "ts" method supports most of the arguments to ts()
    ## but NOT the class argument (!), and it won't work right
    ## if people set up "mts" objects with "ts" class (!, again)
    setMethod("initialize", "ts",
              function(object, data =   NA, start = 1, end = numeric(0), frequency = 1, 
    deltat = 1, ts.eps = getOption("ts.eps"), names = NULL, ...) {
                  if(nargs() < 2) # guaranteed to be called with object from new
                      object
                  else if(is.ts(data) && nargs() == 2 + length(list(...)))
                      .mergeAttrs(data, object, list(...))
                  else {
                      value <- ts(data, start, end, frequency, 
                                  deltat, ts.eps, names = names)
                      .mergeAttrs(value, object, list(...))
                  }
              })
}

## .OldClassList is a purely heuristic list of known old-style classes, with emphasis
## on old-style class inheritiance.  Used in .InitBasicClasses to call setOldClass for
## each known class pattern.
.OldClassesList <-
    list(
         c("anova", "data.frame"),
         c("mlm", "lm"),
         c("POSIXt", "POSIXct"),
         "dump.frames",
         c("ordered", "factor"),
         c("glm.null", "glm", "lm"),
         c("anova.glm.null", "anova.glm"),
         "hsearch",
         "integrate",
         "packageInfo",
         "libraryIQR",
         "packageIQR",
         "mtable",
         "table",
         "summary.table",
         "mts", ## ts is done like matrix, array--is that reasonable?
         "recordedplot",
         "socket",
         "packageIQR",
         "density",
         "formula",
         "logLik",
         "rle"
)
