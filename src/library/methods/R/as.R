as <-
  ## Returns the version of this object coerced to be the given `Class'.
  ## If the corresponding `is' relation is true, it will be used.  In particular,
  ## if the relation has a coerce method, the method will be invoked on `object'.
  ##
  ## If the `is' relation is FALSE, and `coerceFlag' is `TRUE',
  ## the coerce function will be called (which will throw an error if there is
  ## no valid way to coerce the two objects).  Otherwise, `NULL' is returned.
  function(object, Class, coerceFlag = TRUE)
{
    thisClass <- data.class(object)
    if(thisClass == Class)
        return(object)
    if(is(object, Class)) {
        ## look for coerce method or indirection
        coe <- extendsCoerce(thisClass, Class)
        coe(object)
    }
    else if(coerceFlag) {
      asFun <- paste("as", Class, sep=".")
      f <- getFunction(asFun, mustFind = FALSE)
      if(is.null(f))
        stop(paste("No method defined to coerce to class \"",Class, "\""), sep="")
      else
        f(object)
    }
    else
        NULL
}



## .CoerceAll <-
##   function(object, toClass)
## {
##     value <- NULL
##     f <- elNamed(asFunctions, toClass) ## the predefined as.* functions
##     if(is.function(f))
##             value <- f(object)
##     else if(!is.na(match(toClass, .BasicVectorClasses)))
##         value <- as.vector(object, toClass)
##     else if(isClass(toClass)) {
##         value <- NULL
##         fromClass <- data.class(object)
##         if(isClass(fromClass)) {
##             if(identical(slotNames(fromClass), slotNames(toClass)))
##                 value <- object
##         }
##         else if(data.class(getPrototype(getClass(toClass))) == fromClass)
##             value <- object
##     }
##     if(is.null(value))
##         stop(paste("Unable to perform default coerce from \"",
##                    data.class(object), "\" to \"", toClass, "\"", sep=""))
##     class(value) <- toClass
##     value
## }

"as<-" <-
  ## Set the portion of the object defined by the right-hand side.
  ##
  ## Typically, the object being modified extends the class of the right-hand side object,
  ## and contains the slots of that object. These slots (only) will then be replaced.
  function(object, Class, coerce = TRUE, value)
{
    ## TODO:  examine the extends relation to find a replace method.
    ## Meanwhile, handle the case of slot containment
    if(coerce && !identical(data.class(value), Class))
        value <- as(value, Class)
    if(is(object, Class)) {
        slots <- slotNames(Class)
        if(length(slots)>0) {
            for(what in slots)
                slot(object, what) <- slot(value, what)
        }
        ## should check here that this is the only superClass of object
        ## (needs a version of the extends function with 1 arg that uses
        ## only the direct definition, not the completion).
        else {
            class(value) <- Class
            object <- value
        }
    }
    else
        stop("Non-extension version of as(...) <- not yet implemented.")
    object
}


## asFunctions <-
##     list(
##          "array" = function(object, to) as.array(object),
##          "call" = function(object, to) as.call(object),
##          "character" = function(object, to) as.character(object),
##          "complex" = function(object, to) as.complex(object),
##          "dataframe" = function(object, to) as.dataframe(object),
##          "double" = function(object, to) as.double(object),
## 	"environment" = function(object, to) as.environment(object),
##          "expression" = function(object, to) as.expression(object),
##          "factor" = function(object, to) as.factor(object),
##          "formula" = function(object, to) as.formula(object),
##          "function" = function(object, to) as.function(object),
##          "integer" = function(object, to) as.integer(object),
##          "list" = function(object, to) as.list(object),
##          "logical" = function(object, to) as.logical(object),
##          "matrix" = function(object, to) as.matrix(object),
##          "name" = function(object, to) as.name(object),
##          "null" = function(object, to) as.null(object),
##          "numeric" = function(object, to) as.numeric(object),
##          "ordered" = function(object, to) as.ordered(object),
##          "pairlist" = function(object, to) as.pairlist(object),
##          "real" = function(object, to) as.real(object),
##          "single" = function(object, to) as.single(object),
##          "symbol" = function(object, to) as.symbol(object),
##          "table" = function(object, to) as.table(object),
##          "ts" = function(object, to) as.ts(object)
##          )


setAs <- 
  function(from, to, def, where = 1)
  {
    ## where there is an "is" relation, modify it
    if(extends(from, to, TRUE)) {
      extds <- getExtends(getClassDef(from))
      if(is.list(extds))
        test <- elNamed(extds, "test")
      else
        test <- NULL
      setIs(from, to, test = test, coerce = def, where = where)
    }
    else {
      asFun <- paste("as", to, sep=".")
      if(isGeneric(asFun))
        setMethod(asFun, from, def, where = where)
      else {
        if(!existsFunction(asFun))
          assign(asFun, function(object) stop("No default coerce method"), where)
        setGeneric(asFun, where = where)
        setMethod(asFun, from, def, where = where)
      }
     }
  }

.setCoerceGeneric <- function(where) {
  ## doesn't do anything at the moment.  Will go away if the current version
  ## of as() pans out.
}
