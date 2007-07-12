as <-
  ## Returns the version of this object coerced to be the given `Class'.
  ## If the corresponding `is' relation is true, it will be used.  In particular,
  ## if the relation has a coerce method, the method will be invoked on `object'.
  ##
  ## If the `is' relation is FALSE, and `coerceFlag' is `TRUE',
  ## the coerce function will be called (which will throw an error if there is
  ## no valid way to coerce the two objects).  Otherwise, `NULL' is returned.
  function(object, Class, strict = TRUE, ext = possibleExtends(thisClass, Class))
{
    thisClass <- .class1(object) ## always one string
    if(.identC(thisClass, Class) || .identC(Class, "ANY"))
        return(object)
    where <- .classEnv(thisClass)
    coerceFun <- getGeneric("coerce", where = where)
    coerceMethods <- getMethodsForDispatch("coerce", coerceFun)
    asMethod <- .quickCoerceSelect(thisClass, Class, coerceFun, coerceMethods)
    if(is.null(asMethod)) {
        sig <-  c(from=thisClass, to = Class)
        packageSlot(sig) <- where
        ## try first for an explicit (not inherited) method
        ## ?? Can this ever succeed if .quickCoerceSelect failed?
        asMethod <- selectMethod("coerce", sig, TRUE,
                                 FALSE, #optional, no inheritance
                                 fdef = coerceFun, mlist = coerceMethods)
        canCache <- TRUE
        if(is.null(asMethod)) {
            inherited <- FALSE
            if(is(object, Class)) {
                ClassDef <- getClassDef(Class, where)
                ## use the ext information, computed or supplied
                if(identical(ext, FALSE))
                    stop(gettextf("internal problem in as(): \"%s\" is(object, \"%s\") is TRUE, but the metadata asserts that the 'is' relation is FALSE", thisClass, Class), domain = NA)
                else if(identical(ext, TRUE))
                    asMethod <- .makeAsMethod(quote(from), TRUE, Class, ClassDef, where)
                else {
                  test <- ext@test
                  asMethod <- .makeAsMethod(ext@coerce, ext@simple, Class, ClassDef, where)
                  canCache <- (!is(test, "function")) || identical(body(test), TRUE)
                 }
            }
            if(is.null(asMethod) && extends(Class, thisClass)) {
                ClassDef <- getClassDef(Class, where)
                asMethod <- .asFromReplace(thisClass, Class, ClassDef, where)
            }
            ## if none of these applies, look for an inherited method
            ## but only on the from argument
            if(is.null(asMethod)) {
                asMethod <- selectMethod("coerce", sig, TRUE,
                                         c(from = TRUE, to = FALSE),
                                         fdef = coerceFun, mlist =
                                         coerceMethods)
                inherited <- TRUE
            }
            else if(canCache)  # make into method definition
                asMethod <- .asCoerceMethod(asMethod, sig, FALSE)
            ## cache in the coerce function's environment
            if(canCache && !is.null(asMethod)) {
                cacheMethod("coerce", sig, asMethod, fdef = coerceFun,
                            inherited = inherited)
            }
        }
    }
    if(is.null(asMethod))
        stop(gettextf("no method or default for coercing \"%s\" to \"%s\"", thisClass, Class), domain = NA)
    else if(strict)
        asMethod(object)
    else
        asMethod(object, strict = FALSE)
}

.quickCoerceSelect <- function(from, to, fdef, methods) {
    if(is.null(methods))
        return(NULL)
    else if(is.environment(methods)) {
      ##FIXME:  this may fail if sig's need package informatiion as
      ##well.  Problem is that we would like new("signature.",...)
      ##here but that fails when methods package is booting
        label <- .sigLabel(c(from, to))
        if(exists(label, envir = methods, inherits = FALSE))
          get(label, envir = methods)
        else
          NULL
    }
    else {
        allMethods <- methods@allMethods
        i <- match(from, names(allMethods))
        if(is.na(i))
          NULL
        else {
            methodsi <- allMethods[[i]]
            j <- match(to, names(methodsi))
            if(is.na(j))
              NULL
            else
              methodsi[[j]]
        }
    }
}

.asFromReplace <- function(fromClass, toClass, ClassDef, where) {
    ## toClass extends fromClass, so an asMethod will
    ## be the equivalent of new("toClass", fromObject)
    ## But must check that replacement is defined, in the case
    ## of nonstandard superclass relations
    replaceMethod <- elNamed(ClassDef@contains, fromClass)
    if(is(replaceMethod, "SClassExtension") &&
       !identical(as(replaceMethod@replace, "function"), .ErrorReplace)) {
        f <- function(from, to) NULL
        body(f, envir = where) <-
            substitute({obj <- new(TOCLASS); as(obj, FROMCLASS) <- from; obj},
                       list(FROMCLASS = fromClass, TOCLASS = toClass))
        f
    }
    else
        NULL

}



"as<-" <-
  ## Set the portion of the object defined by the right-hand side.
  ##
  ## Typically, the object being modified extends the class of the right-hand side object,
  ## and contains the slots of that object. These slots (only) will then be replaced.
  function(object, Class, value) {
    thisClass <- .class1(object)
    if(!.identC(.class1(value), Class))
      value <- as(value, Class)
    where <- .classEnv(class(object))
    coerceFun <- getGeneric("coerce<-", where = where)
    coerceMethods <- getMethodsForDispatch("coerce<-", coerceFun)
    asMethod <- .quickCoerceSelect(thisClass, Class, coerceFun, coerceMethods)
    if(is.null(asMethod)) {
        sig <-  c(from=thisClass, to = Class)
        canCache <- TRUE
        inherited <- FALSE
        asMethod <- selectMethod("coerce<-", sig, TRUE, FALSE, #optional, no inheritance
                                 fdef = coerceFun, mlist = coerceMethods)
        if(is.null(asMethod)) {
            if(is(object, Class)) {
                asMethod <- possibleExtends(thisClass, Class)
                if(identical(asMethod, TRUE)) {# trivial, probably identical classes
                    class(value) <- class(object)
                    return(value)
                }
                else {
                    test <- asMethod@test
                    asMethod <- asMethod@replace
                    canCache <- (!is(test, "function")) || identical(body(test), TRUE)
                    if(canCache) { ##the replace code is a bare function
                        asMethod <- .asCoerceMethod(asMethod, sig, TRUE)
                    }
                }
            }
            else { # search for inherited method
              asMethod <- selectMethod("coerce<-", sig, TRUE, c(from = TRUE, to = FALSE))
              inherited <- TRUE
            }
        }
        ## cache for next call
        if(canCache && !is.null(asMethod))
                 cacheMethod("coerce<-", sig, asMethod, fdef = coerceFun,
                             inherited = inherited)
     }
    if(is.null(asMethod))
        stop(gettextf("no method or default for as() replacement of \"%s\" with Class=\"%s\"", thisClass, Class), domain = NA)
    asMethod(object, Class, value)
}



setAs <-
  function(from, to, def, replace = NULL, where = topenv(parent.frame()))
{
    ## where there is an "is" relation, modify it
    fromDef <- getClassDef(from, where)
    extds <- possibleExtends(from, to, fromDef)
    if(is(extds, "SClassExtension")) {
        test <- extds@test
        if(is.null(replace))
            replace <- extds@replace
        test <- NULL
        setIs(from, to, test = test, coerce = def, replace = replace, where = where)
    }
    else if(identical(extds, TRUE)) {
        if(.identC(from, to))
            stop(gettextf("trying to set an 'as' relation from \"%s\" to itself", .class1(from)), domain = NA)
        ## usually to will be a class union, where setAs() is not
        ## allowed by the definition of a union
        toDef <- getClassDef(to, where=where)
        if(is.null(toDef))
            stop(gettextf("class \"%s\" is not defined in this environment", to), domain = NA)
        if(isClassUnion(toDef))
            stop(gettextf("class \"%s\" is a class union: 'coerce' relations to a class union are not meaningful", to), domain = NA)
        ## else go ahead (but are there any cases here where extds is TRUE?)
        setIs(from, to, coerce = def, replace = replace, where = where)
    }
    ## else extds is FALSE -- no is() action
        args <- formalArgs(def)
        if(!is.na(match("strict", args))) args <- args[-match("strict", args)]
        if(length(args) == 1)
            def <- substituteFunctionArgs(def, "from", functionName = "coerce")
        else if(length(args) == 2)
            def <- substituteFunctionArgs(def, c("from", "to"), functionName = "coerce")
        else stop(gettextf("'as' method must have one or two arguments, plus optional 'strict'; got (%s)",
                           paste(formalArgs(def), collapse = ", ")),
                  domain = NA)
        method <- as.list(coerce@.Data) # the function def'n, just to get arguments correct
        method$to <- to
        method <- as.function(method)
        body(method, envir = environment(def)) <- body(def)
        setMethod("coerce", c(from, to), method, where = where)
        if(!is.null(replace)) {
            args <- formalArgs(replace)
            if(identical(args, c("from", "to", "value")))
                method <- replace
            else {
                ## if not from an extends object, process the arguments
                if(length(args) != 2)
                    stop(gettextf("a 'replace' method definition in 'setAs' must be a function of two arguments, got %d", length(args)), domain = NA)
                replace <- body(replace)
                if(!identical(args, c("from", "value"))) {
                    ll <- list(quote(from), quote(value))
                    names(ll) <- args
                    replace <- substituteDirect(replace, ll)
                    warning(gettextf("argument names in replace changed to agree with 'coerce<-' generic:\n%s", paste(deparse(replace), sep="\n    ")),
                            domain = NA)
                }
                method <- eval(function(from, to, value)NULL)
                functionBody(method, envir = .GlobalEnv) <- replace
            }
            setMethod("coerce<-", c(from, to), method, where = where)
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
      switch(what,
	     array =, matrix = body(method, envir = environment(method)) <-
	     substitute({
		 value <- AS(from)
		 if(strict) {
		     dm <- dim(value)
		     dn <- dimnames(value)
		     attributes(value) <- NULL
		     dim(value) <- dm
		     dimnames(value) <- dn
		 }
		 value
	     }, list(AS = as.name(paste("as.", what, sep="")))),
	     ##
	     ts = body(method, envir = environment(method)) <- quote({
		 value <- as.ts(from)
		 if(strict) {
		     attributes(value) <- NULL
		     class(value) <- class(new("ts"))
		     tsp(value) <- tsp(from)
		 }
		 value
	     }),
	     ## default: no attributes
	     body(method, envir = environment(method)) <- substitute({
		 value <- AS(from)
		 if(strict)
		     attributes(value) <- NULL
		 value
	     }, list(AS = as.name(paste("as.", what, sep=""))))
	     )
      setMethod("coerce", c("ANY", what), method, where = where)
  }
  ## and some hand-coded ones
  body(method) <- quote(as.null(from))
  setMethod("coerce", c("ANY", "NULL"), method, where = where)
  body(method) <- quote({
            if(length(from) != 1)
              warning("ambiguous object (length!=1) to coerce to \"name\"")
            as.name(from)
        })
  setMethod("coerce", c("ANY","name"), method, where = where)
  ## not accounted for and maybe not needed:  real, pairlist, double
}

.basicCoerceMethod <- function(from, to, strict = TRUE) stop("undefined coerce method")

.makeAsMethod <- function(expr, simple, Class, ClassDef, where) {
    if(is(expr, "function")) {
        where <- environment(expr)
        args <- formalArgs(expr)
        if(!identical(args, "from"))
            expr <- .ChangeFormals(expr,
                    if(length(args) > 1) .simpleExtCoerce else .simpleIsCoerce)
        expr <- body(expr)
    }
    ## commented code below is needed if we don't assume asMethod sets the class correctly
#     if(isVirtualClass(ClassDef))
#         value <- expr
#     else if(identical(expr, quote(from)))
#         value <- substitute({class(from) <- CLASS; from},
#                            list(CLASS = Class))
#     else value <- substitute({from <- EXPR; class(from) <- CLASS; from},
#                            list(EXPR = expr, CLASS = Class) )
    ## else
    value <- expr
    if(simple && !identical(expr, quote(from)))
        value <- substitute(if(strict) EXPR else from,
                           list(EXPR = expr))
    f <- .simpleExtCoerce
    body(f, envir = where) <- value
    f
}

## check for and remove a previous coerce method.  Called from setIs
## We warn if the previous method seems to be from a
## setAs(), indicating a conflicting setIs() A previous
## version of setIs is OK, but we remove the methods anyway to be safe.
## Definitions are only removed from the current function's environment,
## not from a permanent copy.
.removePreviousCoerce <- function(from, to, where, prevIs) {
    sig <- c(from, to)
    cdef <- getGeneric("coerce", where = where)
    if(is.null(cdef))
        return(FALSE) # only for booting the methods package?
    prevCoerce <- !is.null(selectMethod("coerce", sig, TRUE, FALSE,
                                      fdef = cdef))
    rdef <- getGeneric("coerce<-", where = where)
    if(is.null(rdef))
        return(FALSE) # only for booting the methods package?
    prevRepl <- !is.null(selectMethod("coerce<-", sig, TRUE, FALSE,
                                      fdef = rdef))
    if(prevCoerce || prevRepl) {
        if(!prevIs)
            warning(gettextf("methods currently exist for coercing from \"%s\" to \"%s\"; they will be replaced.", from, to), domain = NA)
        if(prevCoerce)
            setMethod(cdef, sig, NULL, where = baseenv())
        if(prevRepl)
            setMethod(rdef, sig, NULL, where = baseenv())
        TRUE
    }
    else
        FALSE

}

canCoerce <- function(object, Class) {
    is(object, Class) ||
    !is.null(selectMethod("coerce", c(class(object), Class),
			  optional = TRUE,
			  useInherited = c(from=TRUE, to=FALSE)))
}

## turn raw function into method for coerce() or coerce<-()
## Very primitive to survive bootstrap stage, so includes knowledge of
## the classes and does no checking.
.asCoerceMethod <- function(def, sig, replace) {
  if(replace)
    value <- function(from, to, value)NULL
  else
    value <- function(from, to, strict = TRUE) NULL
  body(value) <- body(def)
    value = new("MethodDefinition")
    value@.Data <- def
    classes <- new("signature")
    classes@.Data <- sig
    classes@names <- c("from", "to")
        value@target <- classes
        value@defined <- classes
    value
}
