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
    thisClass <- data.class(object) ## always one string
    if(thisClass == Class)
        return(object)
    if(is(object, Class)) {
        ## look for coerce method or indirection
        coe <- extendsCoerce(thisClass, Class)
        coe(object)
    }
    else if(coerceFlag) {
      sig <- c(from = thisClass, to = Class)
      asMethod <- selectMethod("coerce", sig, TRUE, c(TRUE, FALSE))
      if(is.null(asMethod)) {
        for(byClass in names(getExtends(getClass(thisClass)))) {
          sig[[1]] <- byClass
          asMethod <- selectMethod("coerce", sig, TRUE, c(TRUE, FALSE))
          if(!is.null(asMethod)) {
            ## (TO DO:  need a way to cache this method for the session)
            asMethod <- substitute(function(object) {
              method2 <- eval(METHOD2, .GlobalEnv)
              method2(as(object, BYCLASS))
            }, list(METHOD2 = asMethod, BYCLASS = byClass))
            asMethod <- eval(asMethod, .GlobalEnv)
            break
          }
        }
        if(is.null(asMethod))
          stop(paste("No method or default for coercing \"", thisClass,
                     "\" to \"", Class, "\"", sep=""))
      }
      asMethod(object)
    }
    else
        NULL
}



"as<-" <-
  ## Set the portion of the object defined by the right-hand side.
  ##
  ## Typically, the object being modified extends the class of the right-hand side object,
  ## and contains the slots of that object. These slots (only) will then be replaced.
  function(object, Class, coerceFlag = TRUE, value) {
    thisClass <- data.class(object)
    if(coerceFlag && !identical(data.class(value), Class))
      value <- as(value, Class)
    if(is(object, Class)) {
      f <- extendsReplace(thisClass, Class)
      object <- f(object, value)
    }
    else if(coerceFlag) {
      sig <- c(from = thisClass, to = Class)
      asMethod <- selectMethod("coerce<-", sig, TRUE, c(TRUE, FALSE))
      if(is.null(asMethod))
        stop(paste("No method or default for as() replacement of \"", thisClass,
                   "\" with Class=\"", Class, "\"", sep=""))
      else
        object <- asMethod(object, Class, value)
    }
    else
      stop("Must have coerceFlag=TRUE in this case, since is(object, Class) is FALSE")
    object
}



setAs <- 
  function(from, to, def, replace = NULL, where = 1)
  {
    ## where there is an "is" relation, modify it
    if(extends(from, to, TRUE)) {
      extds <- getExtends(getClassDef(from))
      if(is.list(extds)) {
        test <- elNamed(extds, "test")
        if(missing(replace))
          replace <- elNamed(extds, "replace")
      }
      else
        test <- NULL
      setIs(from, to, test = test, coerce = def, replace = replace, where = where)
    }
    else {
      args <- formalArgs(def)
      if(length(args) != 1)
        stop("a method definition in setAs must be a function of one argument")
      def <- body(def)
      if(!identical(args, "from")) {
        ll <- list(quote(from))
        names(ll) <- args
        def <- substituteDirect(def, ll)
      }
      method <- eval(function(from, to)NULL)
      functionBody(method, envir = .GlobalEnv) <- def
      setMethod("coerce", c(from, to), method, where = where)
      if(!is.null(replace)) {
        args <- formalArgs(replace)
        if(length(args) != 2)
          stop("a replace method definition in setAs must be a function of two arguments")
        replace <- body(replace)
        ll <- list(quote(from), quote(value))
        names(ll) <- args
        replace <- substituteDirect(replace, ll)
        method <- eval(function(from, to, value)NULL)
        functionBody(method, envir = .GlobalEnv) <- replace
        setMethod("coerce<-", c(from, to), method, where = where)
      }
    }
  }

.setCoerceGeneric <- function(where) {
  ## create the initial version of the coerce function, with methods that convert
  ## arbitrary objects to the basic classes by calling the corresponding as.<Class>
  ## functions.
  setGeneric("coerce", function(from, to)standardGeneric("coerce"), where = where)
  setGeneric("coerce<-", function(from, to, value)standardGeneric("coerce<-"), where = where)
  basics <- c(
 "POSIXct",  "POSIXlt",  "array",  "call",  "character",  "complex",  "data.frame", "double", 
 "environment",  "expression",  "factor",  "formula",  "function",  "integer", 
 "list",  "logical",  "matrix",  "name",  "null",  "numeric",  "ordered", 
 "pairlist",  "real",  "single",  "symbol",  "table",  "ts",  "vector")
  for(what in basics) {
    method <- eval(substitute(function(from, to)AS(from),
                              list(AS = as.name(paste("as.", what, sep="")))),
                   .GlobalEnv)
    setMethod("coerce", c("ANY", what), method, where = where)
  }
}
