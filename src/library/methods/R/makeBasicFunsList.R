## the executable code to complete the list of basic functions.

## uses the primitive list and the function .addBasicFunction
## defined (earlier) in BasicFunsList.S

.makeBasicFunsList<- function(where)
{
    env <- new.env()
    .BasicFunsList <- get(".BasicFunsList", envir=where)
    ## the Math group.

    for(f in c("log", "sqrt", "log10", "cumprod", "abs", "acos", "acosh", "asin", "asinh",
               "atan", "atanh", "ceiling", "cos", "cosh", "cumsum", "exp", "floor",
               "gamma", "lgamma", "sin", "sinh", "tan", "tanh", "trunc"))
        .BasicFunsList <- .addBasicGeneric(.BasicFunsList, f, function(x)standardGeneric(""),"Math", env)


    invisible(setGeneric(where=where,"Math", function(x)stop("Don't call this generic directly")))

    ## The Math2 group (too late now, but it would probably have been better to put "trunc" in
    ## here and call this something like the "Truncation" group)

    .BasicFunsList <- .addBasicGeneric(.BasicFunsList, "round",
                                       function (x, digits = 0)  standardGeneric(""), "Math2", env)
    .BasicFunsList <- .addBasicGeneric(.BasicFunsList, "signif",
                                       function (x, digits = 6)  standardGeneric(""), "Math2", env)

    invisible(setGeneric(where=where,"Math2", function(x, digits)stop("Don't call this generic directly"),
                         group = "Ops"))


    ## The Arith group

    for(f in c("+", "-", "*", "^", "%%", "%/%", "/"))
        .BasicFunsList <- .addBasicGeneric(.BasicFunsList, f, function(e1, e2) standardGeneric(""),
                                           "Arith", env)

    invisible(setGeneric(where=where,"Arith", function(e1, e2)stop("Don't call this generic directly"),
                         group = "Ops"))

    ## the Compare group

    for(f in c("==", ">", "<", "!=", "<=", ">="))
        .BasicFunsList <- .addBasicGeneric(.BasicFunsList, f, function(e1, e2) standardGeneric(""),
                                           "Compare", env)

    invisible(setGeneric(where=where,"Compare", function(e1, e2)stop("Don't call this generic directly"),
                         group = "Ops"))

    ## R does not currently do the Logic group (! & |) A strange group, since the argument
    ## lists are different.  But perhaps we'll add it sometime.

    ## the Ops group generic

    invisible(setGeneric(where=where,"Ops", function(e1, e2)stop("Don't call this generic directly")))

    ## The Summary group

    ## These are a bit problematic, since they essentially have "..." as their only data-related
    ## formal argument.  The treatment since S3 has been to define the generic with a special
    ## first argument, to allow method dispatch.  But the method had better invoke the generic
    ## recursively or perform some other special computations, in order to avoid unintended
    ## anomalies, such as !identical(max(x,y), max(y,x))

    for(f in c("max", "min", "range", "prod", "sum", "any", "all"))
        .BasicFunsList <- .addBasicGeneric(.BasicFunsList, f, function (x, ..., na.rm = FALSE) standardGeneric(""),
                                           "Summary", env)

    invisible(setGeneric(where=where,"Summary", function (x, ..., na.rm = FALSE)stop("Don't call this generic directly"),
                         group = "Ops"))

    ## The Complex group
    ##
    ## R adds this group to the previous S language function groups, for all the operations
    ## defined on complex numbers.  For applications wanting to define a new class that
    ## extends the concept of complex numbers, a function group is likely to be useful
    ## since all these functions may operate in a similar manner (by analogy, e.g., with the
    ## Math group).

    for(f in c("Arg", "Conj", "Im", "Mod", "Re"))
        .BasicFunsList <- .addBasicGeneric(.BasicFunsList, f, function(z) standardGeneric(""),
                                           "Complex", env)

    invisible(setGeneric(where=where,"Complex", function(z)stop("Don't call this generic directly"),
                         group = "Ops"))

    assign(".BasicFunsList", .BasicFunsList, envir=where)
}

