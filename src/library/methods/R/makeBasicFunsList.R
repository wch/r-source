## the executable code to complete the generics corresponding to primitives,
## and to define the group generics for these functions.

## uses the primitive list and the function .addBasicGeneric
## defined (earlier) in BasicFunsList.R

.makeBasicFuns <- function(where)
{
    funs <- get(".BasicFunsList", envir=where)

    ## First, set up the existing functions in the list as valid generics.
    ## This will override anything except the S4 group generics.
    curNames <- names(funs)
    for(i in seq_along(funs)) {
	val <- funs[[i]]
        if (is.function(val))
            funs <- .addBasicGeneric(funs, curNames[[i]], val, "")
    }

    ## Next, add the remaining primitive generics
    prims <- ls(.GenericArgsEnv, all.names=TRUE)
    new_prims <- prims[!prims %in% names(funs)]
    for(nm in new_prims) {
        f <- get(nm, envir = .GenericArgsEnv)
        body(f) <- substitute(standardGeneric(ff), list(ff=val))
        funs <- .addBasicGeneric(funs, nm, f, "")
    }

    ## Then add all the primitives that are not already there.
    ff <- ls("package:base", all.names=TRUE)
    prims <- ff[sapply(ff, function(x) is.primitive(get(x, "package:base")))]
    new_prims <- prims[!prims %in% names(funs)]
    add <- rep(list(FALSE), length(new_prims))
    names(add) <- new_prims
    funs <- c(funs, add)

    ## the Math group.
    members <- c("abs", "sign", "sqrt",
                 "ceiling", "floor", "trunc",
                 "cummax", "cummin", "cumprod", "cumsum",
		 "exp", "expm1",
		 "log", "log10", "log2", "log1p",
                 "cos", "cosh", "sin", "sinh", "tan", "tanh",
                 "acos", "acosh", "asin", "asinh", "atan", "atanh",
                 "gamma", "lgamma", "digamma", "trigamma"
                 )
     for(f in members) {
         funs <-
             if(f %in% c("log", "trunc"))
                 .addBasicGeneric(funs, f,
                                  function(x, ...) standardGeneric(""),
                                  "Math")

             else
                 .addBasicGeneric(funs, f,
                                  function(x) standardGeneric(""),
                                  "Math")
    }

    setGroupGeneric(where=where,"Math", function(x)NULL,
		    knownMembers = members, package = "base")

    ## The Math2 group.
    funs <- .addBasicGeneric(funs, "round",
			     function(x, digits = 0) standardGeneric(""),
			     "Math2")
    funs <- .addBasicGeneric(funs, "signif",
			     function(x, digits = 6) standardGeneric(""),
			     "Math2")

    setGroupGeneric(where = where, "Math2", function(x, digits) NULL,
		    knownMembers = c("round", "signif"), package = "methods")

    ## The Arith group
    members <- c("+", "-", "*", "^", "%%", "%/%", "/")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
				 "Arith")

    setGroupGeneric(where = where, "Arith", function(e1, e2)NULL,
		    group = "Ops", knownMembers = members, package = "base")

    ## the Compare group
    members <- c("==", ">", "<", "!=", "<=", ">=")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
				 "Compare")

    setGroupGeneric(where = where, "Compare", function(e1, e2)NULL,
		    group = "Ops", knownMembers = members, package = "methods")

    ## The Logic group
    members <- c("&", "|") ## *not*  "!" since that has only one argument
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
				 "Logic")
    setGroupGeneric(where = where, "Logic", function(e1, e2) NULL,
		    group = "Ops", knownMembers = members, package = "base")

    ## the Ops group generic

    setGroupGeneric(where = where,"Ops", function(e1, e2) NULL,
		    knownMembers = c("Arith", "Compare", "Logic"),
                    package = "base")


    ## The Summary group

    ## These are a bit problematic, since they essentially have "..."
    ## as their only data-related formal argument.  The treatment
    ## since S3 has been to define the generic with a special first
    ## argument, to allow method dispatch.  But the method had better
    ## invoke the generic recursively or perform some other special
    ## computations, in order to avoid unintended anomalies, such as
    ## !identical(max(x,y), max(y,x))

    members <- c("max", "min", "range", "prod", "sum", "any", "all")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function (x, ..., na.rm = FALSE)
				 standardGeneric(""),
				 "Summary")

    setGroupGeneric(where = where, "Summary",
		    function(x, ..., na.rm = FALSE) NULL,
		    knownMembers = members, package = "base")

    ## The Complex group

    ## R adds this group to the previous S language function groups,
    ## for all the operations defined on complex numbers.  For
    ## applications wanting to define a new class that extends the
    ## concept of complex numbers, a function group is likely to be
    ## useful since all these functions may operate in a similar
    ## manner (by analogy, e.g., with the Math group).

    members <- c("Arg", "Conj", "Im", "Mod", "Re")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(z) standardGeneric(""),
				 "Complex")

    setGroupGeneric(where=where,"Complex", function(z)NULL,
		    knownMembers = members, package = "base")

    assign(".BasicFunsList", funs, envir=where)
    rm(.addBasicGeneric, envir=where)
}

.BasicFunsMethods <- list()

.InitSubsetMethods <- function(where)
{
    ## create some methods for the functions in .BasicFunsList.

    ## The strategy on initialization is to create the methods list
    ## objects by setMethod calls, and then move them to the
    ## .BasicFunsMethods list, from which they will be retrieved when
    ## the generic is installed; see getGeneric.

    ## The mlist objects can not be left in the methods library itself
    ## after initialization, since the basic functions are not "turned
    ## on" as generics until some other package or user defines
    ## methods for them.

    ## This facility is not currently instantiated, though there are
    ## some desirable changes to the "[" and related functions, to
    ## untangle the current messing with "drop" and multiple
    ## subscripts, in the vector case.
}
