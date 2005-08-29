## lists of functions and expressions used in dispatch of functions
## defined internally (as .Primitive's) for which formal argument lists
## are not available, or for which a generic, if created, needs to have a special
## form (e.g., belonging to one of the predefined groups of functions).

.BasicFunsList <-
list("!" = function(e1)
{
    standardGeneric("!")
}
, "$" = function(x, name)
{
    name <- as.character(substitute(name))
    standardGeneric("$")
}
, "$<-" = function(x, name, value)
{
    name <- as.character(substitute(name))
    standardGeneric("$<-")
}
, "%*%" = function(x, y)
{
    standardGeneric("%*%")
}
, "&" = function(e1, e2)
{
    standardGeneric("&")
}
, "&&" = function(e1, e2)
{
    standardGeneric("&&")
}
, ".C" = FALSE
, ".Call" = FALSE
, ".Fortran" = FALSE
, ":" = function(e1, e2)
{
    standardGeneric(":")
}
, "<-" = function(x, value)
{
    standardGeneric("<-")
}
, "<<-" = function(x, value)
{
    standardGeneric("<<-")
}
, "UseMethod" = FALSE
, "[" = function(x, i, j, ..., drop)
{
    standardGeneric("[")
}
, "[<-" = function(x, i, j, ..., value)
{
    standardGeneric("[<-")
}
, "[[" = function(x, i, j, ...)
{
    standardGeneric("[[")
}
, "[[<-" = function(x, i, j, ..., value)
{
    standardGeneric("[[<-")
}
, "attr" = function(x, which)
{
    standardGeneric("attr")
}
, "attr<-" = function(x, which, value)
{
    standardGeneric("attr<-")
}
, "attributes<-" = function(x, value)
{
    standardGeneric("attributes<-")
}
, "browser" = function(object, ...)
{
    standardGeneric("browser")
}
, "c" = function(x, ..., recursive = FALSE)
{
    standardGeneric("c")
}
, "call" = function(x, ...)
{
    standardGeneric("call")
}
, "class<-" = function(x, value)
{
    standardGeneric("class<-")
}
, "dim<-" = function(x, value)
{
    standardGeneric("dim<-")
}
, "dimnames<-" = function(x, value)
{
    standardGeneric("dimnames<-")
}
, "expression" = function(x, ...)
{
    standardGeneric("expression")
}
, "is.loaded" = function(symbol)
{
    standardGeneric("is.loaded")
}
, "length<-" = function(x, value)
{
    standardGeneric("length<-")
}
, "list" = function(x, ...)
{
    standardGeneric("list")
}
, "missing" = FALSE
, "on.exit" = FALSE
, "substitute" = FALSE
, "symbol.C" = FALSE
, "symbol.For" = FALSE
, "trace" = function(what = character(), tracer = TRUE, exit.tracer, at = numeric(), print
	 = TRUE, signature)
{
    standardGeneric("trace")
}
, "untrace" = function(what)
{
    standardGeneric("untrace")
}
, "|" = function(e1, e2)
{
    standardGeneric("|")
}
, "||" = function(e1, e2)
{
    standardGeneric("||")
}
, "~" = function(x, y)
{
    standardGeneric("~")
}
, "as.call" = function(x)
{
    standardGeneric("as.call")
}
, "as.character" = function(x)
{
    standardGeneric("as.character")
}
, "attributes" = function(x)
{
    standardGeneric("attributes")
}
, "class" = function(x)
{
    standardGeneric("class")
}
, "cummax" = function(x)
{
    standardGeneric("cummax")
}
, "cummin" = function(x)
{
    standardGeneric("cummin")
}
, "cumprod" = function(x)
{
    standardGeneric("cumprod")
}
, "dim" = function(x)
{
    standardGeneric("dim")
}
, "dimnames" = function(x)
{
    standardGeneric("dimnames")
}
, "invisible" = function(x)
{
    standardGeneric("invisible")
}
, "is.array" = function(x)
{
    standardGeneric("is.array")
}
, "is.atomic" = function(x)
{
    standardGeneric("is.atomic")
}
, "is.call" = function(x)
{
    standardGeneric("is.call")
}
, "is.character" = function(x)
{
    standardGeneric("is.character")
}
, "is.complex" = function(x)
{
    standardGeneric("is.complex")
}
, "is.double" = function(x)
{
    standardGeneric("is.double")
}
, "is.expression" = function(x)
{
    standardGeneric("is.expression")
}
, "is.finite" = function(x)
{
    standardGeneric("is.finite")
}
, "is.function" = function(x)
{
    standardGeneric("is.function")
}
, "is.infinite" = function(x)
{
    standardGeneric("is.infinite")
}
, "is.integer" = function(x)
{
    standardGeneric("is.integer")
}
, "is.language" = function(x)
{
    standardGeneric("is.language")
}
, "is.list" = function(x)
{
    standardGeneric("is.list")
}
, "is.logical" = function(x)
{
    standardGeneric("is.logical")
}
, "is.matrix" = function(x)
{
    standardGeneric("is.matrix")
}
, "is.na" = function(x)
{
    standardGeneric("is.na")
}
, "is.name" = function(x)
{
    standardGeneric("is.name")
}
, "is.nan" = function(x)
{
    standardGeneric("is.nan")
}
, "is.null" = function(x)
{
    standardGeneric("is.null")
}
, "is.numeric" = function(x)
{
    standardGeneric("is.numeric")
}
, "is.recursive" = function(x)
{
    standardGeneric("is.recursive")
}
, "is.single" = function(x)
{
    standardGeneric("is.single")
}
, "length" = function(x)
{
    standardGeneric("length")
}
, "sign" = function(x)
{
    standardGeneric("sign")
}
, "unclass" = function(x)
{
    standardGeneric("unclass")
}
, "(" = FALSE
, ".Call.graphics" = FALSE
, ".External" = FALSE
, ".External.graphics" = FALSE
, ".Internal" = FALSE
, ".Primitive" = FALSE
, "baseenv" = FALSE
, "break" = FALSE
, "debug" = function(fun)
{
    standardGeneric("debug")
}
, "environment<-" = function(fun, value)
{
    standardGeneric("environment<-")
}
, "for" = FALSE
, "function" = FALSE
, "gc.time" = FALSE
, "globalenv" = FALSE
, "if" = FALSE
, "interactive" = FALSE
, "is.environment" = function(obj)
{
    standardGeneric("is.environment")
}
, "is.object" = function(x)
{
    standardGeneric("is.object")
}
, "is.pairlist" = function(x)
{
    standardGeneric("is.pairlist")
}
, "is.real" = function(x)
{
    standardGeneric("is.real")
}
, "is.symbol" = function(x)
{
    standardGeneric("is.symbol")
}
, "nargs" = FALSE
, "next" = FALSE
, "pos.to.env" = FALSE
, "proc.time" = FALSE
, "repeat" = FALSE
, "return" = FALSE
, "undebug" = function(fun)
{
    standardGeneric("undebug")
}
, "while" = FALSE
, "{" = FALSE
)

## the names of the basic funs with the style of "["
## R implements these in an inconsistent call mechanism, in which missing arguments
## are allowed, and significant, but argument names are not used.  See callNextMethod

.BasicSubsetFunctions <- c("[", "[[", "[<-", "[[<-")

## create generic functions corresponding to the basic (primitive) functions
## but don't leave them as generics in the package.  Instead store them in
## a named list to be used by setMethod, w/o forcing method dispatch on these
## functions.

.addBasicGeneric <-
    function(funslist, f, fdef, group = list(), env)
{
    deflt <- get(f, "package:base")
    ## use the arguments of the base package function
    ##FIXME:  should also deal with the functions having ... as the first
    ## argument, but needs to create a generic with different args from the deflt
    ## => constructing a call to the base function from the default
    if(is.primitive(deflt)) {
        body(fdef, envir = baseenv()) <-
            substitute(standardGeneric(FNAME, DEFLT), list(FNAME=f, DEFLT=deflt))
    }
    else {
        fdef <- deflt
        body(fdef, envir = baseenv()) <-
            substitute(standardGeneric(FNAME), list(FNAME=f))
    }
    deflt <- .derivedDefaultMethod(deflt)
    elNamed(funslist, f) <- makeGeneric(f, fdef, deflt, group = group, package = "base")
    funslist
}

.ShortPrimitiveSkeletons <-
    list( quote(f(x,i)), quote(fgets(x,i,value=value)))

.EmptyPrimitiveSkeletons <-
    list( quote(f(x)), quote(fgets(x,value=value)))

## utilities to get and set the primitive generics.
## Version below uses the environment, not the list
## in order to work with namespace for methods pacakge
# genericForPrimitive <- function(f, where = topenv(parent.frame())) {
#     what <- methodsPackageMetaName("G", f)
#     if(exists(what, where))
#         get(what, where)
#     else
#         NULL
# }

# setGenericForPrimitive <-function(f, value, where = topenv(parent.frame()))
#     assign(methodsPackageMetaName("G", f), value, where)

## temporary versions while primitives are still handled by a global
## table

genericForPrimitive <- function(f, where = topenv(parent.frame())) {
    if(.matchBasic(f, .ExcludePrimitiveGenerics, FALSE))
        stop(gettextf("methods may not be defined for primitive function '%s' in this version of R", f), domain = NA)
    env <- .findBasicFuns(where)
    funs <- get(".BasicFunsList", envir = env)
    elNamed(funs, f)
}

setGenericForPrimitive <- function(f, value, where = topenv(parent.frame()),
                                   methods = getMethods(value)) {
    env <- .findBasicFuns(where)
    funs <- get(".BasicFunsList", envir = env)
    if(is.null(elNamed(funs, f)))
        stop(gettextf("'%s' is not one of the basic functions", f), domain = NA)
    elNamed(funs, f) <- value
    assign(".BasicFunsList", funs, envir = env)
    if(is(methods, "MethodsList") && is.primitive(get(f, "package:base")))
        setPrimitiveMethods(f, finalDefaultMethod(methods), "set", value, methods)
    f
}

.findBasicFuns <- function(where) {
    allWhere <- .findAll(".BasicFunsList", where = where)
    if(length(allWhere) == 0)
        as.environment("package:methods")
    else
        as.environment(allWhere[[1]])
}

.ExcludePrimitiveGenerics <-
    c(
      "is.null",
      "is.primitive",
      "is.function",
      "is.object"
      )
