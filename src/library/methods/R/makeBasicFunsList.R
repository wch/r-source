## the executable code to complete the list of basic functions.

## uses the primitive list and the function .addBasicFunction
## defined (earlier) in BasicFunsList.S

.makeBasicFunsList<- function(where)
{
    env <- new.env()
    funs <- get(".BasicFunsList", envir=where)
    ## First, set up the existing functions in the list as valid generics.
    curNames <- names(funs)
    for(i in seq(along=funs)) {
      val <- funs[[i]]
      if(is.function(val))
        funs <- .addBasicGeneric(funs, curNames[[i]], val, "", env)
    }
      
    ## the Math group.
    members <- c("log", "sqrt", "log10", "cumprod", "abs", "acos", "acosh", "asin", "asinh",
               "atan", "atanh", "ceiling", "cos", "cosh", "cumsum", "exp", "floor",
               "gamma", "lgamma", "sin", "sinh", "tan", "tanh", "trunc")
    for(f in members)
        funs <- .addBasicGeneric(funs, f, function(x)standardGeneric(""),"Math", env)


    setGroupGeneric(where=where,"Math", function(x)NULL,
                              knownMembers = members)

    ## The Math2 group (too late now, but it would probably have been better to put "trunc" in
    ## here and call this something like the "Truncation" group)

    funs <- .addBasicGeneric(funs, "round",
                                       function (x, digits = 0)  standardGeneric(""), "Math2", env)
    funs <- .addBasicGeneric(funs, "signif",
                                       function (x, digits = 6)  standardGeneric(""), "Math2", env)

    setGroupGeneric(where=where,"Math2", function(x, digits)NULL,
                              knownMembers = c("round", "signif"))

    ## The Arith group
    members <- c("+", "-", "*", "^", "%%", "%/%", "/")
    for(f in members)
        funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
                                           "Arith", env)

    setGroupGeneric(where=where,"Arith", function(e1, e2)NULL,
                              group = "Ops", knownMembers = members)

    ## the Compare group
    members <- c("==", ">", "<", "!=", "<=", ">=")
    for(f in members)
        funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
                                           "Compare", env)

    setGroupGeneric(where=where,"Compare", function(e1, e2)NULL,
                              group = "Ops", knownMembers = members)

    ## R does not currently do the Logic group (! & |) A strange group, since the argument
    ## lists are different.  But perhaps we'll add it sometime.

    ## the Ops group generic

    setGroupGeneric(where=where,"Ops", function(e1, e2)NULL,
                               knownMembers = c("Arith", "Compare"))


    ## The Summary group

    ## These are a bit problematic, since they essentially have "..." as their only data-related
    ## formal argument.  The treatment since S3 has been to define the generic with a special
    ## first argument, to allow method dispatch.  But the method had better invoke the generic
    ## recursively or perform some other special computations, in order to avoid unintended
    ## anomalies, such as !identical(max(x,y), max(y,x))
    members <- c("max", "min", "range", "prod", "sum", "any", "all")
    for(f in members)
        funs <- .addBasicGeneric(funs, f, function (x, ..., na.rm = FALSE) standardGeneric(""),
                                           "Summary", env)

    setGroupGeneric(where=where,"Summary", function(x, ..., na.rm = FALSE )NULL,
                              knownMembers = members)

    ## The Complex group
    ##
    ## R adds this group to the previous S language function groups, for all the operations
    ## defined on complex numbers.  For applications wanting to define a new class that
    ## extends the concept of complex numbers, a function group is likely to be useful
    ## since all these functions may operate in a similar manner (by analogy, e.g., with the
    ## Math group).
    members <- c("Arg", "Conj", "Im", "Mod", "Re")
    for(f in members)
        funs <- .addBasicGeneric(funs, f, function(z) standardGeneric(""),
                                           "Complex", env)

    setGroupGeneric(where=where,"Complex", function(z)NULL,
                              knownMembers = members)

    assign(".BasicFunsList", funs, envir=where)
    rm(.addBasicGeneric, envir=where)
}

