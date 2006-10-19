## the executable code to complete the generics corresponding to primitives,
## and to define the group generics for these functions.

## uses the primitive list and the function .addBasicGeneric
## defined (earlier) in BasicFunsList.R

.makeBasicFuns<- function(where)
{
    env <- new.env(hash=TRUE, parent=as.environment(where))
    funs <- get(".BasicFunsList", envir=where)
    ## First, set up the existing functions in the list as valid generics.
    curNames <- names(funs)
    for(i in seq(along=funs)) {
	val <- funs[[i]]
	if(is.function(val))
	    funs <- .addBasicGeneric(funs, curNames[[i]], val, "", env)
    }
    ## the Math group.
    members <- c("log", "sqrt", "log10", "cumprod", "abs", "acos",
		 "acosh", "asin", "asinh", "atan", "atanh",
		 "ceiling", "cos", "cosh", "cumsum", "exp", "floor",
		 "gamma", "lgamma", "sin", "sinh", "tan", "tanh")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(x) standardGeneric(""),
				 "Math", env)

    setGroupGeneric(where=where,"Math", function(x)NULL,
		    knownMembers = members, package = "base")

    ## The Math2 group.  In spite of the documented behavior, there
    ## is an S3 method for trunc with a SECOND argument.  Until we get
    ## a better solution, we have to put trunc in the Math2 group.

    funs <- .addBasicGeneric(funs, "trunc",
			     function(x, digits = 0) standardGeneric(""),
			     "Math2", env)

    funs <- .addBasicGeneric(funs, "round",
			     function(x, digits = 0) standardGeneric(""),
			     "Math2", env)
    funs <- .addBasicGeneric(funs, "signif",
			     function(x, digits = 6) standardGeneric(""),
			     "Math2", env)

    setGroupGeneric(where = where, "Math2", function(x, digits) NULL,
		    knownMembers = c("round", "signif"), package = "methods")

    ## The Arith group
    members <- c("+", "-", "*", "^", "%%", "%/%", "/")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
				 "Arith", env)

    setGroupGeneric(where = where, "Arith", function(e1, e2)NULL,
		    group = "Ops", knownMembers = members, package = "base")

    ## The Logic group
    members <- c("&", "|") ## *not*  "!" since that has only one argument
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
				 "Logic", env)
    setGroupGeneric(where = where, "Logic", function(e1, e2) NULL,
		    group = "Ops", knownMembers = members, package = "base")

    ## the Compare group
    members <- c("==", ">", "<", "!=", "<=", ">=")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
				 "Compare", env)

    setGroupGeneric(where = where, "Compare", function(e1, e2)NULL,
		    group = "Ops", knownMembers = members, package = "methods")

    ## R does not currently do the Logic group (! & |)
    ## A strange group, since the argument lists are different.	 But
    ## perhaps we'll add it sometime.


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
				 "Summary", env)

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
				 "Complex", env)

    setGroupGeneric(where=where,"Complex", function(z)NULL,
		    knownMembers = members, package = "base")

    assign(".BasicFunsList", funs, envir=where)
    assign(".BasicFunsEnv", env, envir=where)
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
