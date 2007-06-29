## top-level assignments that need to be copied to baseloader.R
as.numeric <- as.real <- as.double
is.name <- is.symbol


## extracted from existing NAMESPACE files in Dec 2003
.knownS3Generics <- local({

    ## include the S3 group generics here
    baseGenerics <- c("Math", "Ops", "Summary", "Complex",
        "as.character", "as.data.frame", "as.matrix", "as.vector",
        "cbind", "labels", "print", "rbind", "rep", "seq", "seq.int",
        "solve", "summary", "t")

    utilsGenerics <- c("edit", "str")

    graphicsGenerics <- c("contour", "hist", "identify", "image",
        "lines", "pairs", "plot", "points", "text")

    statsGenerics <- c( "add1", "AIC", "anova", "biplot", "coef",
        "confint", "deviance", "df.residual", "drop1", "extractAIC",
        "fitted", "formula", "logLik", "model.frame", "model.matrix",
        "predict", "profile", "qqnorm", "residuals", "se.contrast",
        "terms", "update", "vcov")

    tmp <- rep.int(c("base", "utils", "graphics", "stats"),
                   c(length(baseGenerics), length(utilsGenerics),
                     length(graphicsGenerics), length(statsGenerics)))
    names(tmp) <-
        c(baseGenerics, utilsGenerics, graphicsGenerics, statsGenerics)
    tmp
})

.ArgsEnv <- new.env(hash = TRUE, parent = emptyenv())

assign("%*%", function(x, y) NULL, envir = .ArgsEnv)
assign(".C", function(name, ..., NAOK = FALSE, DUP = TRUE, PACKAGE,
                      ENCODING) NULL,
       envir = .ArgsEnv)
assign(".Fortran", function(name, ..., NAOK = FALSE, DUP = TRUE, PACKAGE,
                            ENCODING) NULL,
       envir = .ArgsEnv)
assign(".Call", function(name, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".Call.graphics", function(name, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".External", function(name, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".External.graphics", function(name, ..., PACKAGE) NULL,
       envir = .ArgsEnv)
assign(".Internal", function(call) NULL, envir = .ArgsEnv)
assign(".Primitive", function(name) NULL, envir = .ArgsEnv)
assign(".primTrace", function(obj) NULL, envir = .ArgsEnv)
assign(".primUntrace", function(obj) NULL, envir = .ArgsEnv)
assign(".subset", function(x, ...) NULL, envir = .ArgsEnv)
assign(".subset2", function(x, ...) NULL, envir = .ArgsEnv)
assign("as.call", function(x) NULL, envir = .ArgsEnv)
assign("as.environment", function(object) NULL, envir = .ArgsEnv)
assign("attr", function(x, which, exact = FALSE) NULL, envir = .ArgsEnv)
assign("attr<-", function(x, which, value) NULL, envir = .ArgsEnv)
assign("attributes", function(obj) NULL, envir = .ArgsEnv)
assign("attributes<-", function(obj, value) NULL, envir = .ArgsEnv)
assign("baseenv", function() NULL, envir = .ArgsEnv)
assign("browser", function() NULL, envir = .ArgsEnv)
assign("call", function(name, ...) NULL, envir = .ArgsEnv)
assign("class", function(x) NULL, envir = .ArgsEnv)
assign("class<-", function(x, value) NULL, envir = .ArgsEnv)
assign("debug", function(fun) NULL, envir = .ArgsEnv)
assign("emptyenv", function() NULL, envir = .ArgsEnv)
assign("environment<-", function(fun, value) NULL, envir = .ArgsEnv)
assign("expression", function(...) NULL, envir = .ArgsEnv)
assign("gc.time", function(on = TRUE) NULL, envir = .ArgsEnv)
assign("globalenv", function() NULL, envir = .ArgsEnv)
assign("interactive", function() NULL, envir = .ArgsEnv)
assign("invisible", function(x) NULL, envir = .ArgsEnv)
assign("is.atomic", function(x) NULL, envir = .ArgsEnv)
assign("is.call", function(x) NULL, envir = .ArgsEnv)
assign("is.character", function(x) NULL, envir = .ArgsEnv)
assign("is.complex", function(x) NULL, envir = .ArgsEnv)
assign("is.double", function(x) NULL, envir = .ArgsEnv)
assign("is.environment", function(x) NULL, envir = .ArgsEnv)
assign("is.expression", function(x) NULL, envir = .ArgsEnv)
assign("is.function", function(x) NULL, envir = .ArgsEnv)
assign("is.integer", function(x) NULL, envir = .ArgsEnv)
assign("is.language", function(x) NULL, envir = .ArgsEnv)
assign("is.list", function(x) NULL, envir = .ArgsEnv)
assign("is.logical", function(x) NULL, envir = .ArgsEnv)
assign("is.name", function(x) NULL, envir = .ArgsEnv)
assign("is.null", function(x) NULL, envir = .ArgsEnv)
assign("is.object", function(x) NULL, envir = .ArgsEnv)
assign("is.pairlist", function(x) NULL, envir = .ArgsEnv)
assign("is.raw", function(x) NULL, envir = .ArgsEnv)
assign("is.real", function(x) NULL, envir = .ArgsEnv)
assign("is.recursive", function(x) NULL, envir = .ArgsEnv)
assign("is.single", function(x) NULL, envir = .ArgsEnv)
assign("is.symbol", function(x) NULL, envir = .ArgsEnv)
assign("list", function(...) NULL, envir = .ArgsEnv)
assign("lazyLoadDBfetch", function(key, file, compressed, hook) NULL,
       envir = .ArgsEnv)
assign("missing", function(x) NULL, envir = .ArgsEnv)
assign("nargs", function() NULL, envir = .ArgsEnv)
assign("nzchar", function(x) NULL, envir = .ArgsEnv)
assign("oldClass", function(x) NULL, envir = .ArgsEnv)
assign("oldClass<-", function(x, value) NULL, envir = .ArgsEnv)
assign("on.exit", function(expr, add = FALSE) NULL, envir = .ArgsEnv)
assign("pos.to.env", function(x) NULL, envir = .ArgsEnv)
assign("proc.time", function() NULL, envir = .ArgsEnv)
assign("quote", function(expr) NULL, envir = .ArgsEnv)
assign("retracemem", function(x, previous = NULL) NULL, envir = .ArgsEnv)
assign("seq_along", function(along.with) NULL, envir = .ArgsEnv)
assign("seq_len", function(length.out) NULL, envir = .ArgsEnv)
assign("standardGeneric", function(f) NULL, envir = .ArgsEnv)
assign("storage.mode<-", function(x, value) NULL, envir = .ArgsEnv)
assign("substitute", function(expr, env) NULL, envir = .ArgsEnv)
assign("tracemem", function(x) NULL, envir = .ArgsEnv)
assign("unclass", function(x) NULL, envir = .ArgsEnv)
assign("undebug", function(fun) NULL, envir = .ArgsEnv)
assign("untracemem", function(x) NULL, envir = .ArgsEnv)
assign("UseMethod", function(generic, object) NULL, envir = .ArgsEnv)


.S3PrimitiveGenerics <-
    c("as.character", "as.complex", "as.double", "as.integer",
    "as.logical", "as.numeric", "as.raw", "as.real", "c", "dim",
    "dim<-", "dimnames", "dimnames<-", "is.array", "is.finite",
    "is.infinite", "is.matrix",
    "is.na", "is.nan", "is.numeric", "length", "length<-", "levels<-",
    "names", "names<-", "rep", "seq.int")

.GenericArgsEnv <- local({
    env <- new.env(hash = TRUE, parent = emptyenv())
    for(f in .S3PrimitiveGenerics) {
        fx <- function(x) {}
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }
    ## now add the group generics
    ## round, signif are not primitive
    ## log, trunc are handled below
    fx <- function(x) {}
    for(f in c('abs', 'sign', 'sqrt', 'floor', 'ceiling',
               'exp', 'expm1', 'log1p', 'log10', 'log2',
               'cos', 'sin', 'tan', 'acos', 'asin', 'atan', 'cosh', 'sinh',
               'tanh', 'acosh', 'asinh', 'atanh',
               'gamma', 'lgamma', 'digamma', 'trigamma',
               'cumsum', 'cumprod', 'cummax', 'cummin')) {
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }

    ## ! is unary and handled below
    fx <- function(e1, e2) {}
    for(f in c('+', '-', '*', '/', '^', '%%', '%/%', '&', '|',
               '==', '!=', '<', '<=', '>=', '>')) {
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }

    ## none of Summary is primitive
    for(f in c("Arg", "Conj", "Im", "Mod", "Re")) {
        fx <- function(z) {}
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }
    env
})
### do these outside to get the base namespace as the environment.
assign("!", function(x) UseMethod("!"), envir = .GenericArgsEnv)
assign("as.character", function(x, ...) UseMethod("as.character"),
       envir = .GenericArgsEnv)
assign("as.complex", function(x, ...) UseMethod("as.complex"),
       envir = .GenericArgsEnv)
assign("as.double", function(x, ...) UseMethod("as.double"),
       envir = .GenericArgsEnv)
assign("as.integer", function(x, ...) UseMethod("as.integer"),
       envir = .GenericArgsEnv)
assign("as.logical", function(x, ...) UseMethod("as.logical"),
       envir = .GenericArgsEnv)
assign("as.raw", function(x) UseMethod("as.raw"), envir = .GenericArgsEnv)
assign("c", function(..., recursive = FALSE) UseMethod("c"),
       envir = .GenericArgsEnv)
assign("dimnames", function(x) UseMethod("dimnames"), envir = .GenericArgsEnv)
assign("dim<-", function(x, value) UseMethod("dim<-"), envir = .GenericArgsEnv)
assign("dimnames<-", function(x, value) UseMethod("dimnames<-"),
       envir = .GenericArgsEnv)
assign("length<-", function(x, value) UseMethod("length<-"),
       envir = .GenericArgsEnv)
assign("levels<-", function(x, value) UseMethod("levels<-"),
       envir = .GenericArgsEnv)
assign("log", function(x, base=exp(1)) UseMethod("log"),
       envir = .GenericArgsEnv)
assign("names<-", function(x, value) UseMethod("names<-"),
       envir = .GenericArgsEnv)
assign("rep", function(x, ...) UseMethod("rep"), envir = .GenericArgsEnv)
assign("seq.int", function(from, to, by, length.out, along.with, ...)
       UseMethod("seq.int"), envir = .GenericArgsEnv)
assign("trunc", function(x, ...) UseMethod("trunc"), envir = .GenericArgsEnv)

## make these the same object as as.double
assign("as.numeric", get("as.double", envir = .GenericArgsEnv),
       envir = .GenericArgsEnv)
assign("as.real", get("as.double", envir = .GenericArgsEnv),
       envir = .GenericArgsEnv)
