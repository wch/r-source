## Lists of functions and expressions used in dispatch of functions
## defined internally (as .Primitive's) for which formal argument lists
## are not available, or for which a generic, if created,
## needs to have a special form (e.g., belonging to one of the
## predefined groups of functions).

## The list is expanded in .makeBasicFuns by adding the group generics.
## Only the primitives are listed here.

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
### S4 generic via R_possible_dispatch in do_matprod
{
    standardGeneric("%*%")
}
, "&" = function(e1, e2)
{
    standardGeneric("&")
}
, "&&" = FALSE
, "(" = FALSE
, ":" = FALSE
, "@" = FALSE
, "{" = FALSE
, "<-" = FALSE
, "<<-" = FALSE
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
, "|" = function(e1, e2)
{
    standardGeneric("|")
}
, "||" = FALSE
, "~" = FALSE
, "=" = FALSE
, ".C" = FALSE
, ".Call" = FALSE
, ".Call.graphics" = FALSE
, ".External" = FALSE
, ".External.graphics" = FALSE
, ".Fortran" = FALSE
, ".Internal" = FALSE
, ".Primitive" = FALSE
, ".primTrace" = FALSE
, ".primUntrace" = FALSE
, ".subset" = FALSE
, ".subset2" = FALSE
, "UseMethod" = FALSE
, "as.call" = FALSE
, "as.character" = function(x, ...)
{
    standardGeneric("as.character")
}
, "as.complex" = function(x, ...)
{
    standardGeneric("as.complex")
}
, "as.environment" = FALSE
, "as.integer" = function(x, ...)
{
    standardGeneric("as.integer")
}
, "as.logical" = function(x, ...)
{
    standardGeneric("as.logical")
}
, "as.numeric" = function(x, ...)
{
    standardGeneric("as.numeric")
}
, "as.raw" = function(x)
{
    standardGeneric("as.raw")
}
, "attr" = FALSE
, "attr<-" = FALSE
, "attributes" = FALSE
, "attributes<-" = FALSE
, "baseenv" = FALSE
, "break" = FALSE
, "browser" = FALSE
, "c" = function(x, ..., recursive = FALSE)
{
    standardGeneric("c")
}
, "call" = FALSE
, "class" = FALSE
, "class<-" = FALSE
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
, "debug" = FALSE
, "dim" = function(x)
{
    standardGeneric("dim")
}
, "dim<-" = function(x, value)
{
    standardGeneric("dim<-")
}
, "dimnames" = function(x)
{
    standardGeneric("dimnames")
}
, "dimnames<-" = function(x, value)
{
    standardGeneric("dimnames<-")
}
, "emptyenv" = FALSE
, "environment<-" = FALSE
, "expression" = FALSE
, "for" = FALSE
, "function" = FALSE
, "gc.time" = FALSE
, "globalenv" = FALSE
, "if" = FALSE
, "interactive" = FALSE
, "invisible" = FALSE
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
, "is.null" = FALSE
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
, "length<-" = function(x, value)
{
    standardGeneric("length<-")
}
, "levels<-" = function(x, value)
{
    standardGeneric("levels<-")
}
, "list" = FALSE
, "missing" = FALSE
, "names" = function(x)
{
    standardGeneric("names")
}
, "names<-" = function(x, value)
{
    standardGeneric("names<-")
}
, "nargs" = FALSE
, "next" = FALSE
, "oldClass" = FALSE
, "oldClass<-" = FALSE
, "on.exit" = FALSE
, "pos.to.env" = FALSE
, "proc.time" = FALSE
, "quote" = FALSE
, "rep" = function(x, ...)
{
    standardGeneric("rep")
}
, "repeat" = FALSE
, "retracemem" = FALSE
, "return" = FALSE
, "seq.int" = FALSE
, "seq_along" = FALSE
, "seq_len" = FALSE
, "sign" = function(x)
{
    standardGeneric("sign")
}
, "standardGeneric" = FALSE
, "storage.mode<-" = FALSE
, "substitute" = FALSE
, "tracemem" = FALSE
, "unclass" = FALSE
, "undebug" = FALSE
, "untracemem" = FALSE
, "while" = FALSE
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

## temporary versions while primitives are still handled by a global table

genericForPrimitive <- function(f, where = topenv(parent.frame())) {
#    if(.matchBasic(f, .ExcludePrimitiveGenerics, FALSE))
#        stop(gettextf("methods may not be defined for primitive function \"%s\" in this version of R", f), domain = NA)
    env <- .findBasicFuns(where)
    funs <- get(".BasicFunsList", envir = env)
    ans <- elNamed(funs, f)
    ## this element may not exist (yet, during loading), dom't test null
    if(identical(ans, FALSE))
        stop(gettextf("methods may not be defined for primitive function \"%s\" in this version of R", f), domain = NA)
    ans
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
