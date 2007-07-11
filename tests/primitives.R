## check that the 'internal generics' are indeed generic.

x <- structure(pi, class="testit")
xx <- structure("OK", class="testOK")

for(f in ls(.GenericArgsEnv, all.names=TRUE))
{
    cat("testing S3 generic '", f, "'\n", sep="")
    method <- paste(f, "testit", sep=".")
    if(f %in% "seq.int") {
        ## note that this dispatches on 'seq'.
        assign("seq.testit", function(...) xx, .GlobalEnv)
        res <- seq.int(x, x)
    } else {
        if(length(grep("<-$", f)) > 0) {
            assign(method, function(x, value) xx, .GlobalEnv)
            y <- x
            res <- eval(substitute(ff(y, value=pi), list(ff=as.name(f))))
        } else if(f == "log") {
            assign(method, function(x, base) xx, .GlobalEnv)
            res <- eval(substitute(ff(x), list(ff=as.name(f))))
        } else {
            assign(method, function(x) xx, .GlobalEnv)
            res <- eval(substitute(ff(x), list(ff=as.name(f))))
        }
    }
    stopifnot(res == xx)
    rm(method)
}

## and that no others are generic
for(f in ls(.ArgsEnv, all.names=TRUE))
{
    if(f == "browser") next
    cat("testing non-generic '", f, "'\n", sep="")
    method <- paste(f, "testit", sep=".")
    fx <- get(f, envir=.ArgsEnv)
    body(fx) <- quote(return(42))
    assign(method, fx, .GlobalEnv)
    na <- length(formals(fx))
    res <- NULL
    if(na == 1)
        res <- try(eval(substitute(ff(x), list(ff=as.name(f)))), silent = TRUE)
    else if(na == 2)
        res <- try(eval(substitute(ff(x, x), list(ff=as.name(f)))), silent = TRUE)
    if(!inherits(res, "try-error") && identical(res, 42)) stop("is generic")
    rm(method)
}


## check that all primitives are accounted for in .[Generic]ArgsEnv.
## and nothing else
ff <- ls("package:base", all.names=TRUE)
ff <- ff[sapply(ff, function(x) is.primitive(get(x, "package:base")))]
lang_elements <-
    c('$', '$<-', '&&', '(', ':', '<-', '<<-', '=', '@',
      '[', '[<-', '[[', '[[<-', 'break', 'for', 'function', 'if', 'next',
      'repeat', 'return', 'while', '{', '||', '~')

known <- c(ls(.GenericArgsEnv, all.names=TRUE),
           ls(.ArgsEnv, all.names=TRUE),
           lang_elements)
stopifnot(ff %in% known, known %in% ff)


## check which are not considered as possibles for S4 generic
ff4 <- names(methods:::.BasicFunsList)
# as.double and as.real are the same as as.numeric
S4generic <- ff %in% c(ff4, "as.double", "as.real")
notS4 <- ff[!S4generic]
if(length(notS4))
    cat("primitives not covered in methods:::.BasicFunsList:",
        paste(sQuote(notS4), collapse=", "), "\n")
stopifnot(S4generic)

# functions which are listed but not primitive
extraS4 <- c('all', 'any', 'max', 'min', 'prod', 'range',
             'round', 'signif', 'sum')
ff4[!ff4 %in% c(ff, extraS4)]
stopifnot(ff4 %in% c(ff, extraS4))


## primitives which are not internally generic cannot have S4 methods
## unless specifically arranged (e.g. %*%)
nongen_prims <- ff[!ff %in% ls(.GenericArgsEnv, all.names=TRUE)]
ff3 <- names(methods:::.BasicFunsList)[sapply(methods:::.BasicFunsList, function(x) is.logical(x) && !x)]
ex <- nongen_prims[!nongen_prims %in% c("$", "$<-", "[", "[[" ,"[[<-", "[<-", "%*%", ff3)]
if(length(ex))
    cat("non-generic primitives not excluded in methods:::.BasicFunsList:",
        paste(sQuote(ex), collapse=", "), "\n")
stopifnot(length(ex) == 0)

## Now check that (most of) those which are listed really are generic.
require(methods)
setClass("foo", representation(x="numeric", y="numeric"))
xx <- new("foo",  x=1, y=2)
S4gen <- names(methods:::.BasicFunsList)[sapply(methods:::.BasicFunsList, function(x) is.function(x))]
for(f in S4gen) {
    cat("testing '", f, "'  ", sep="")
    g <- get(f)
    if(is.primitive(g)) g <- getGeneric(f) # should error on non-Generics.
    ff <- args(g)
    body(ff) <- "testit"
    nm <- names(formals(ff))
    na <- length(nm[nm != "..."])
    ## only test one or two args for now
    ## the Math2 and Summary groups give problems
    if(na < 1 || na > 2 || nm[1] == '...' || f %in% c("round", "signif")) {
        cat("skipping\n")
        next
    }
    setMethod(f, "foo", ff)
    stopifnot(identical(g(xx), "testit"))
    cat("OK\n")
}
