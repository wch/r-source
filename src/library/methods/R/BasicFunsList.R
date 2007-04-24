## lists of functions and expressions used in dispatch of functions
## defined internally (as .Primitive's) for which formal argument lists
## are not available, or for which a generic, if created, needs to have a special
## form (e.g., belonging to one of the predefined groups of functions).

.BasicFunsList <-
list("!" = function(x)
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
# not internally generic
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
, "[" = function(x, i, j, ..., drop = TRUE)
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
, "names<-" = function(x, value)
{
    standardGeneric("names<-")
}
, "expression" = function(x, ...)
{
    standardGeneric("expression")
}
, "length<-" = function(x, value)
{
    standardGeneric("length<-")
}
, "levels<-" = function(x, value)
{
    standardGeneric("levels<-")
}
, "list" = function(x, ...)
{
    standardGeneric("list")
}
, "missing" = FALSE
, "on.exit" = FALSE
, "rep" = function(x, ...)
{
    standardGeneric("rep")
}
, "substitute" = FALSE
# not primitive
, "trace" = function(what = character(), tracer = TRUE, exit.tracer, at = numeric(), print
	 = TRUE, signature)
{
    standardGeneric("trace")
}
# not primitive
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
, "as.character" = function(x, ...)
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
, "names" = function(x)
{
    standardGeneric("names")
}
, "invisible" = function(x)
{
    standardGeneric("invisible")
}
, "is.array" = function(x)
{
    standardGeneric("is.array")
}
, "is.atomic" = FALSE
, "is.call" = FALSE
, "is.character" = FALSE
, "is.complex" = FALSE
, "is.double" = FALSE
, "is.environment" = FALSE
, "is.expression" = FALSE
, "is.finite" = FALSE
, "is.function" = FALSE
, "is.infinite" = FALSE
, "is.integer" = FALSE
, "is.language" = FALSE
, "is.list" = FALSE
, "is.logical" = FALSE
, "is.matrix" = function(x)
{
    standardGeneric("is.matrix")
}
, "is.na" = function(x)
{
    standardGeneric("is.na")
}
, "is.name" = FALSE
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
, "is.object" = FALSE
, "is.pairlist" = FALSE
, "is.raw" = FALSE
, "is.real" = FALSE
, "is.recursive" = FALSE
, "is.single" = FALSE
, "is.symbol" = FALSE
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
# not internal generic
, "debug" = function(fun)
{
    standardGeneric("debug")
}
, "emptyenv" = FALSE
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
, "nargs" = FALSE
, "next" = FALSE
, "pos.to.env" = FALSE
, "proc.time" = FALSE
, "repeat" = FALSE
, "return" = FALSE
# not internal generic
, "undebug" = function(fun)
{
    standardGeneric("undebug")
}
, "while" = FALSE
, "{" = FALSE
, "=" = FALSE
, "@" = FALSE
, ".primTrace" = FALSE
, ".primUntrace" = FALSE
, ".subset" = FALSE
, ".subset2" = FALSE
, "as.environment" = FALSE
, "oldClass" = FALSE
, "oldClass<-" = FALSE
, "quote" = FALSE
, "retracemem" = FALSE
, "seq.int" = FALSE
, "seq_along" = FALSE
, "seq_len" = FALSE
, "standardGeneric" = FALSE
, "tracemem" = FALSE
, "untracemem" = FALSE
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
        body(fdef, envir = globalenv()) <-
            substitute(standardGeneric(FNAME, DEFLT), list(FNAME=f, DEFLT=deflt))
    }
    else {
        fdef <- deflt
        body(fdef, envir = globalenv()) <-
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
        stop(gettextf("methods may not be defined for primitive function \"%s\" in this version of R", f), domain = NA)
    env <- .findBasicFuns(where)
    funs <- get(".BasicFunsList", envir = env)
    elNamed(funs, f)
}

setGenericForPrimitive <- function(f, value, where = topenv(parent.frame()),
                                   methods = getMethods(value)) {
    env <- .findBasicFuns(where)
    funs <- get(".BasicFunsList", envir = env)
    if(is.null(elNamed(funs, f)))
        stop(gettextf("\"%s\" is not one of the basic functions", f), domain = NA)
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
      ".C",
      ".Call",
      ".Call.graphics",
      ".External",
      ".External.graphics",
      ".Fortran",
      ".Internal",
      ".Primitive",
      ".primTrace",
      ".primUntrace",
      ".subset",
      ".subset2",
      "UseMethod",
      "as.call",
      "as.environment",
      "attr",
      "attr<-",
      "attributes",
      "attributes<-",
      "baseenv",
      "browser",
      "as.call",
      "call",
      "class",
      "class<-",
      "debug",
      "emptyenv",
      "environment<-",
      "expression",
      "gc.time",
      "globalenv",
      "interactive",
      "invisible",
      "is.atomic",
      "is.call",
      "is.character",
      "is.complex",
      "is.double",
      "is.environment",
      "is.expression",
      "is.finite",
      "is.function",
      "is.infinite",
      "is.integer",
      "is.language",
      "is.list",
      "is.logical",
      "is.name",
      "is.null",
      "is.function",
      "is.object",
      "is.pairlist",
      "is.raw",
      "is.real",
      "is.recursive",
      "is.single",
      "is.symbol",
      "list",
      "missing",
      "nargs",
      "oldClass",
      "oldClass",
      "oldClass<-",
      "on.exit",
      "pos.to.env",
      "proc.time",
      "quote",
      "retracemem",
      "seq.int",
      "seq_along",
      "seq_len",
      "standardGeneric",
      "storage.mode<-",
      "substitute",
      "tracemem",
      "unclass",
      "undebug",
      "untracemem"
      )
