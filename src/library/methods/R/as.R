as <-
  ## Returns the version of this object coerced to be the given `Class'.
  ## If the corresponding `is' relation is true, it will be used.  In particular,
  ## if the relation has a coerce method, the method will be invoked on `object'.
  ##
  ## If the `is' relation is FALSE, and `coerceFlag' is `TRUE',
  ## the coerce function will be called (which will throw an error if there is
  ## no valid way to coerce the two objects).  Otherwise, `NULL' is returned.
  function(object, Class, strict = TRUE)
{
    thisClass <- .class1(object) ## always one string
    if(thisClass == Class)
        return(object)
    sig <-  c(from=thisClass, to = Class)
    ## TO DO: would be nice to make this version of selectMethod fast, since it's only
    ## lookups (no inheritance); also, recognizing the case of a simle extension
    ## could skip calling function(object)object !
    asMethod <- selectMethod("coerce", sig, TRUE, FALSE) #optional, no inheritance
    if(is.null(asMethod)) {
        if(is(object, Class)) {
            asMethod <- possibleExtends(thisClass, Class)
             if(identical(asMethod, FALSE))
                stop(paste("Internal problem in as():  \"", thisClass, "\" extends \"",
                           Class, "\", but no coerce method found", sep=""))
            else if(identical(asMethod, TRUE)) 
                asMethod <- .makeAsMethod(quote(from), TRUE, Class)
             else
                 asMethod <- .makeAsMethod(body(asMethod@coerce), asMethod@simple, Class)
            ## cache for next call
            cacheMethod("coerce", c(from = thisClass, to = Class), asMethod)
        }
        else
            asMethod <- selectMethod("coerce", sig, TRUE, c(from = TRUE, to = FALSE))
    }
    if(is.null(asMethod))
        stop(paste("No method or default for coercing \"", thisClass,
                       "\" to \"", Class, "\"", sep=""))
    else if(strict)
        asMethod(object)
    else
        asMethod(object, strict = FALSE)
}



"as<-" <-
  ## Set the portion of the object defined by the right-hand side.
  ##
  ## Typically, the object being modified extends the class of the right-hand side object,
  ## and contains the slots of that object. These slots (only) will then be replaced.
  function(object, Class, value) {
    thisClass <- .class1(object)
    if(!identical(.class1(value), Class))
      value <- as(value, Class)
    sig <-  c(from=thisClass, to = Class)
    asMethod <- selectMethod("coerce<-", sig, TRUE, FALSE) #optional, no inheritance
    if(is.null(asMethod)) {
        if(is(object, Class)) {
            ## possibleExtends can't return TRUE or FALSE: so it must be an extends
            ## object.
            asMethod <- possibleExtends(thisClass, Class)@replace
            ## cache for next call
            cacheMethod("coerce<-", c(from = thisClass, to = Class), asMethod)
        }
        else
            asMethod <- selectMethod("coerce<-", sig, TRUE, c(from = TRUE, to = FALSE))
    }
    if(is.null(asMethod))
        stop(paste("No method or default for as() replacement of \"", thisClass,
                   "\" with Class=\"", Class, "\"", sep=""))
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
      if(!is.na(match("strict", args))) args <- args[-match("strict", args)]
      if(length(args) == 1)
          def <- substituteFunctionArgs(def, "from")
      else if(length(args) == 2)
          def <- substituteFunctionArgs(def, c("from", "to"))
      else stop(paste("as method must have one or two arguments, plus optional `strict'; got (",
                      paste(formalArgs(def), collapse = ", "), ")", sep=""))
      method <- as.list(coerce@.Data) # the function def'n, just to get arguments correct
      method$to <- to
      method <- as.function(method)
      body(method, envir = environment(def)) <- body(def)
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
                  paste(deparse(bdy), sep="\n    "), "\n")
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
  setGeneric("coerce", function(from, to, strict = TRUE)standardGeneric("coerce"),
             where = where)
  setGeneric("coerce<-", function(from, to, value)standardGeneric("coerce<-"), where = where)
  basics <- c(
 "POSIXct",  "POSIXlt",  "array",  "call",  "character",  "complex",  "data.frame", 
 "environment",  "expression",  "factor",  "formula",  "function",  "integer", 
 "list",  "logical",  "matrix",  "name",  "numeric",  "ordered", 
  "single",  "table",  "ts",  "vector")
  basics <- basics[!is.na(match(basics,.BasicClasses))]
  for(what in basics) {
      ## if the class is a basic class and there exists an as.<class> function,
      ## use it as the coerce method.
      method  <- .basicCoerceMethod
      body(method, envir = environment(method)) <- substitute({
          value <- AS(from)
          if(strict)
              attributes(value) <- NULL
          value
          }, list(AS = as.name(paste("as.", what, sep=""))))
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

.basicCoerceMethod <- function(from, to, strict = TRUE) stop("Undefined coerce method")

.makeAsMethod <- function(expr, simple, Class) {
    if(isVirtualClass(getClass(Class)))
    {}
    else if(identical(expr, quote(from)))
        expr <- substitute({class(from) <- CLASS; from},
                           list(CLASS = Class))
    else expr <- substitute({from <- EXPR; class(from) <- CLASS; from},
                           list(EXPR = expr, CLASS = Class) )
    if(simple)
        expr <- substitute(if(strict) EXPR else from,
                           list(EXPR = expr))
    f <- .simpleExtCoerce
    body(f, envir = environment(f)) <- expr
    f
}
