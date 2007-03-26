## check that the 'internal generics' are indeed generic.

x <- structure(pi, class="testit")
xx <- structure("OK", class="testOK")

for(f in ls(.GenericArgsEnv, all.names=TRUE))
{
    cat("testing '", f, "'\n", sep="")
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
        } else {
            assign(method, function(x) xx, .GlobalEnv)
            res <- eval(substitute(ff(x), list(ff=as.name(f))))
        }
    }
    stopifnot(res == xx)
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


## check which are not (potentially) S4 generic
## there probably should be none.
ff4 <- names(methods::.BasicFunsList)
S4generic <- ff %in% ff4
notS4 <- ff[!S4generic]
if(length(notS4)) cat("primitives not covered in methods::.BasicFunsList:",
                      paste(sQuote(notS4), collapse=", "), "\n")
stopifnot(S4generic)

# functions which are listed but not primitive
extraS4 <- c('all', 'any', 'gamma', 'lgamma', 'log', 'log10', 'max',
             'min', 'prod', 'range', 'round', 'signif', 'sum',
             'trace', 'untrace')
stopifnot(ff4 %in% c(ff, extraS4))
