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
    sig <-  c(from=thisClass, to = Class)
    ## TO DO: would be nice to make this version of selectMethod fast, since it's only
    ## lookups (no inheritance)
    asMethod <- selectMethod("coerce", sig, TRUE, FALSE) #optional, no inheritance
    if(is.null(asMethod)) {
        if(is(object, Class)) {
            ## look for coerce method or indirection
            asMethod <- extendsCoerce(thisClass, Class)
            if(is.function(asMethod)) # cache for next call
                cacheMethod("coerce", c(from = thisClass, to = Class), asMethod)
        }
        if(is.null(asMethod) && coerceFlag)
            asMethod <- selectMethod("coerce", sig, TRUE, c(from = TRUE, to = FALSE))
    }
    if(is.null(asMethod)) {
        if(coerceFlag)
            stop(paste("No method or default for coercing \"", thisClass,
                       "\" to \"", Class, "\"", sep=""))
        else
            NULL
    }
    else
        asMethod(object)
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
    if(coerceFlag || !is(object, Class)) {
        sig <- sigToEnv(list(from=thisClass, to=Class))
        asMethod <- selectMethod("coerce<-", sig, TRUE, c(from = TRUE, to = FALSE))
      ## TO DO:  figure out how inheritance works in this function
      if(!is.null(asMethod))
        return(asMethod(object, Class, value))
    }
    if(is(object, Class)) {
        ## a candidate for replacing the whole object ?
        if(isVirtualClass(Class))
            asMethod <- eval(function(object, Class, value) {
                attributes(value) <- attributes(object)
                value}, .GlobalEnv)
        else
            asMethod <- extendsReplace(thisClass, Class)
    }
    if(is.null(asMethod))
        stop(paste("No method or default for as() replacement of \"", thisClass,
                   "\" with Class=\"", Class, "\"", sep=""))
    cacheMethod("coerce<-", c(from = thisClass, to = Class), asMethod)
    asMethod(object, Class, value)
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
      def <- body(def)
      if(length(args) == 1) {
          if(!identical(args, "from")) {
              ll <- list(quote(from), as.name(args))
              names(ll) <- c(args, "from")
              def <- substituteDirect(def, ll)
              message("Argument name in def changed to \"from\" instead of \"",
                      args, "\"")
          }
      }
      else if(length(args) == 2) {
          if(!identical(args, c("from", "to"))) {
              ll <- list(quote(from), quote(to), as.name(args[[1]]), as.name(args[[2]]))
              names(ll) <- c(args, "from", "to")
              def <- substituteDirect(def, ll)
              message("Argument names in def changed to c(\"from\", \"to\") instead of ",
                      deparse(args))
          }
      }
      else stop(paste("as method must have one or two arguments; got", length(args)))
      method <- as.list(function(from, to)NULL)
      method$to <- to
      method <- as.function(method)
      functionBody(method, envir = .GlobalEnv) <- def
      setMethod("coerce", c(from, to), method, where = where)
      if(!is.null(replace)) {
        args <- formalArgs(replace)
        if(length(args) != 2)
          stop("a replace method definition in setAs must be a function of two arguments")
        replace <- body(replace)
        if(!identical(args, c("from", "value"))) {
          ll <- list(quote(from), quote(value))
          names(ll) <- args
          replace <- substituteDirect(replace, ll)
          warning("Argument names in replace changed to agree with \"coerce<-\" generic:\n",
                  paste(deparse(def), sep="\n    "), "\n")
        }
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
 "POSIXct",  "POSIXlt",  "array",  "call",  "character",  "complex",  "data.frame", 
 "environment",  "expression",  "factor",  "formula",  "function",  "integer", 
 "list",  "logical",  "matrix",  "name",  "numeric",  "ordered", 
  "single",  "table",  "ts",  "vector")
  for(what in basics) {
      method  <- eval(function(from, to)NULL, .GlobalEnv)
      body(method) <- substitute(AS(from),
                              list(AS = as.name(paste("as.", what, sep=""))))
      setMethod("coerce", c("ANY", what), method, where = where)
  }
  ## and some hand-coded ones
  body(method) <- quote(as.null(from))
  setMethod("coerce", c("ANY", "NULL"), method)
  body(method) <- quote({
            if(length(from) != 1)
              warning("ambiguous object (length!=1) to coerce to \"name\"")
            as.name(from)
        })
  setMethod("coerce", c("ANY","name"), method)
  ## not accounted for and maybe not needed:  real, pairlist, double
}
