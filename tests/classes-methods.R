#### Formal classes & methods
#### - Started by moving checks from ./reg-S4.R:
###	all "Matrix" + "late_and_no-relevant-print" now here
#### - No *.Rout.save <==> use stopifnot() etc for testing
#### - Recommended packages allowed, e.g., "Matrix"

## Methods for implicit generic  'norm' (x="ANY", type="missing")
##                              'rcond' (x="ANY", norm="missing")
## tested *before* Matrix is loaded
setClass("zzz", slots = c(x = "NULL"))
setMethod( "norm", c(x = "zzz", type = "character"),
          function (x, type, ...) type)
setMethod("rcond", c(x = "zzz", norm = "character"),
          function (x, norm, ...) norm)
m4 <- list(getMethod( "norm", c(x = "ANY", type = "missing")),
           getMethod("rcond", c(x = "ANY", norm = "missing")), # was Error .... : no method found ....
           selectMethod( "norm", c(x = "zzz", type = "missing")),
           selectMethod("rcond", c(x = "zzz", norm = "missing")))
f4 <- lapply(m4, getDataPart)
x <- new("zzz")
stopifnot(all(vapply(m4, is, FALSE, "MethodDefinition")),
          identical(f4[3:4], f4[1:2]),
          identical( norm(x, "O"), "O"),
          identical( norm(x     ), "O"), # was Error .... : invalid 'x': type "S4"
          identical(rcond(x, "O"), "O"),
          identical(rcond(x     ), "O"), # was Error .... : argument "norm" is missing ....
          removeGeneric( "norm"),
          removeGeneric("rcond"),
          removeClass("zzz"))


## PR#19080:
## getGenerics() when a generic function is defined in more than one
## top level environment (package namespace or global environment)
chkgg <-
function (object = getGenerics(), name = "isDiagonal",
          package = character(0L), noEponym = ".GlobalEnv") {
    ## Test that the set of packages defining generic function 'name'
    ## is exactly 'package' and (only for packages in 'noEponym') that
    ## <function name> != <package name>
    stopifnot(is(object, "ObjectsWithPackage"),
              length(w <- which(object == name)) == length(package),
              setequal((p <- packageSlot(object))[w], package),
              noEponym %notin% p[object == p])
}
chkgg()
if (requireNamespace("Matrix", lib.loc = .Library, quietly = TRUE)) {
    chkgg(package = "Matrix")
    setGeneric("isDiagonal", function (.) standardGeneric("isDiagonal"))
    chkgg(package = c("Matrix", ".GlobalEnv"))
 ## ^^^^^ was Error .... : length(w <- .... is not TRUE
    ## data part of getGenerics() had package names
    ##     c("Matrix", ".GlobalEnv")
    ## in place of function names
    ##     c("isDiagonal", "isDiagonal")
    stopifnot(removeGeneric("isDiagonal"))
    chkgg(package = "Matrix")
}


if(require("Matrix", lib.loc = .Library, quietly = TRUE)) {
    D5. <- Diagonal(x = 5:1)
    D5N <- D5.; D5N[5,5] <- NA
    stopifnot(isGeneric("dim", where=as.environment("package:Matrix")),
	      identical(D5., pmin(D5.)),
	      identical(D5., pmax(D5.)),
	      identical(D5., pmax(D5., -1)),
	      identical(D5., pmin(D5., 7)),
	      inherits((D5.3 <- pmin(D5.+2, 3)), "Matrix"),
	      identical(as.matrix(pmin(D5.+2 , 3)),
			pmin(as.matrix(D5.+2), 3)),
	      identical(pmin(1, D5.), pmin(1, as.matrix(D5.))),
	      identical(D5N, pmax(D5N, -1)),
	      identical(D5N, pmin(D5N, 5)),
	      identical(as.matrix(pmin(D5N +1, 3)),
			pmin(as.matrix(D5N)+1, 3)),
              is.function(Matrix::crossprod), # needed Matrix 1.6-1.1 for R-devel, Sep.2023
	      ##
	      TRUE)

  ## R's internal C  R_check_class_and_super()  was not good enough

    setClass("Z", representation(zz = "list"))
    setClass("C", contains = c("Z", "dgCMatrix"))
    setClass("C2", contains = "C")
    setClass("C3", contains = "C2")
    m <- matrix(c(0,0,2:0), 3,5, dimnames = list(NULL,NULL))
    m2 <- matrix(c(0,0,2:0), 3,5)
    show(mC <- as(m, "dgCMatrix"))
    show(cc <- as(mC, "C"))
    c2 <- as(mC, "C2")
    c3 <- as(mC, "C3")
    stopifnot(
        identical(capture.output(c2),
                  sub("C3","C2", capture.output(c3)))
      , identical(as(cc, "matrix"), m2) # changed for Matrix 1.5-x
      , identical(as(c2, "matrix"), m2)
      , identical(as(c3, "matrix"), m2)
    )
    invisible(lapply(c("Z","C","C2","C3"), removeClass))

  ## classUnion <--> class hierarchy :

    sci <- names(getClass("integer")@contains)
    # These 2 classes have *nothing* to do with Matrix:
    setClass("MyClass")
    ncl <- "NumOrMyClass"
    setClassUnion(ncl, c("numeric", "MyClass"))
    nsci <- names(getClass("integer")@contains)
    ## failed in R <= 3.6.2
    stopifnot(sci %in% nsci, identical(setdiff(nsci, sci), ncl))

    setClassUnion('dMatrixOrMatrix', members = c('dMatrix', 'matrix'))
    ## failed in R <= 3.6.2
    stopifnot("dMatrixOrMatrix" %in% names(getClass("dgCMatrix")@contains))

    invisible(lapply(c("NumOrMyClass", "MyClass", "dMatrixOrMatrix"),
                     removeClass))
} else
    message("skipping tests requiring the Matrix package")


## implicit coercion of S4 object to vector via as.vector() in sub-assignment
setClass("A", representation(stuff="numeric"))
as.vector.A <- function (x, mode="any") x@stuff
v <- c(3.5, 0.1)
a <- new("A", stuff=v)
x <- y <- numeric(10)
x[3:4] <- a
y[3:4] <- v
stopifnot(identical(x, y))

## callNextMethod() was broken when augmenting args of primitive generics
foo <- setClass("foo")
bar <- setClass("bar", contains = "foo")

setMethod("[", "foo",  function(x, i, j, ..., flag = FALSE, drop = FALSE) {
    flag
})

setMethod("[", "bar", function(x, i, j, ..., flag = FALSE, drop = FALSE) {
    callNextMethod()
})

BAR <- new("bar")
stopifnot(identical(BAR[1L], FALSE))
stopifnot(identical(BAR[1L, , flag=TRUE], TRUE))

## avoid infinite recursion on Ops,structure methods
setClass("MyInteger",
         representation("integer")
         )
i <- new("MyInteger", 1L)
m <- matrix(rnorm(300), 30,10)
stopifnot(identical(i*m, m))

## when rematching, do not drop arg with NULL default
setGeneric("genericExtraArg",
           function(x, y, extra) standardGeneric("genericExtraArg"),
           signature="x")

setMethod("genericExtraArg", "ANY", function(x, y=NULL) y)

stopifnot(identical(genericExtraArg("foo", 1L), 1L))

## callNextMethod() was broken for ... dispatch
f <- function(...) length(list(...))
setGeneric("f")
setMethod("f", "character", function(...){ callNextMethod() })
stopifnot(identical(f(1, 2, 3), 3L))
stopifnot(identical(f("a", "b", "c"), 3L))

## ... dispatch was evaluating missing arguments in the generic frame
f <- function(x, ..., a = b) {
    b <- "a"
    a
}
setGeneric("f", signature = "...")
stopifnot(identical(f(a=1), 1))
stopifnot(identical(f(), "a"))

## ensure forwarding works correctly for dots dispatch
f2 <- function(...) f(...)
stopifnot(identical(f2(a=1), 1))


## Error messages occurring during method selection are forwarded
f <- function(x) x
setGeneric("f")
setMethod("f", signature("NULL"), function(x) NULL)
err <- tryCatch(f(stop("this is mentioned")), error = identity)
stopifnot(identical(err$message, "error in evaluating the argument 'x' in selecting a method for function 'f': this is mentioned"))


## Upcasting to an S4 class that extends an old class should return the
## requested S4 class, not just the object's S3 part.
local({
    setOldClass(c("oldClassChildForAs",
                  "oldClassParentForAs",
                  "oldClassGrandParentForAs"))
    setClass("GrandParentShimForAs",
             contains = "oldClassGrandParentForAs")
    setClass("ParentShimForAs",
             contains = c("oldClassParentForAs", "GrandParentShimForAs"))
    setClass("S4ChildForAs",
             slots = list(extra = "character"),
             contains = "ParentShimForAs")

    object <- new("S4ChildForAs",
                  structure(list(),
                            class = c("oldClassParentForAs",
                                      "oldClassGrandParentForAs")),
                  extra = "x")

    parent <- as(object, "ParentShimForAs")
    grandparent <- as(object, "GrandParentShimForAs")

    stopifnot(
        isS4(parent),
        is(parent, "ParentShimForAs"),
        identical(as.character(class(parent)), "ParentShimForAs"),
        isS4(grandparent),
        is(grandparent, "GrandParentShimForAs"),
        identical(as.character(class(grandparent)), "GrandParentShimForAs")
    )
})


## Registering an old class from an existing S4 class should update class
## union subclass caches after the old class definition itself is available.
local({
    where <- environment()
    setClass("UnionMemberForOldClassRecache", contains = "VIRTUAL", where = where)
    setClassUnion("UnionForOldClassRecache", "UnionMemberForOldClassRecache",
                  where = where)
    setClass("ParentForOldClassRecache",
             contains = c("UnionForOldClassRecache", "VIRTUAL"), where = where)
    setClass("ChildForOldClassRecache",
             contains = c("ParentForOldClassRecache", "VIRTUAL"), where = where)
    setOldClass(c("ChildForOldClassRecache", "oldClass"),
                S4Class = "ChildForOldClassRecache", where = where)

    union <- getClass("UnionForOldClassRecache", where = where)
    child <- getClass("ChildForOldClassRecache", where = where)
    stopifnot(
        "ChildForOldClassRecache" %in% names(union@subclasses),
        "UnionForOldClassRecache" %in% names(child@contains)
    )
})


## canCoerce(obj, .)  when length(class(obj)) > 1 :
setOldClass("foo")
setAs("foo", "A", function(from) new("A", foo=from))
o3 <- structure(1:7, class = c("foo", "bar"))
stopifnot( canCoerce(o3, "A") )
## failed in R <= 3.6.1


## Error message of  "unable to find .. method" now lists signature argument names:
setGeneric("BaseGeneric", \(x, y, ...) standardGeneric("BaseGeneric"))
setMethod("BaseGeneric", signature(x = "numeric", y = "numeric"), \(x,y, ...) x + y)
errXY <- try(BaseGeneric(X = 1, Y = 2))
err1  <- try(BaseGeneric(1))
err1Y <- try(BaseGeneric(1, Y = 2))
stopifnot(exprs = {
    identical(3, BaseGeneric(1, 2))
    inherits(errXY, "try-error")
    grepl('x = "missing", y = "missing"', attr(errXY,"condition")$message)
    inherits(err1,  "try-error")
    grepl('x = "numeric", y = "missing"', attr(err1, "condition")$message)
    identical(err1, err1Y) # (as $call is empty)
})


## PR#17496: sealClass()
setClass("foo", slots = c(name = "character"), sealed = TRUE)
stopifnot(isSealedClass("foo"))
tools::assertError(setClass("foo"))
stopifnot(removeClass("foo"))
setClass("foo")
sealClass("foo") # failed in R < 4.5.0
stopifnot(isSealedClass("foo"))
stopifnot(removeClass("foo"))


## show(<non-syntactic name>) should recommend backticks for showMethods()
stopifnot(any(grepl("showMethods(`body<-`)", capture.output(show(`body<-`)), fixed=TRUE)))

## show( <genericFunction>)  should use correct " Use  showMethods(.....) for ...."
pkg <- "Matrix"
if(length(P <- grep(pkg, search(), fixed=TRUE, value=TRUE)))
    detach(P, character.only=TRUE, force=TRUE)
(hasME <- requireNamespace(pkg, quietly=TRUE, lib.loc = .Library))
if(hasME) {
    capture.output( show(Matrix::"diag<-") ) |> tail(1) -> out1
    stopifnot(grepl("showMethods(Matrix::`diag<-`)", out1, fixed=TRUE))
    ##
    if(require(pkg, character.only=TRUE)) {
        capture.output( show(Matrix::"diag<-") ) |> tail(1) -> out1
        stopifnot(grepl("showMethods(`diag<-`)", out1, fixed=TRUE))
        detach(paste0("package:", pkg), character.only=TRUE, unload=TRUE)
    } else
        unloadNamespace(pkg)
}


## trace(), debug() etc for  coerce methods -- PR#18823
trr <- quote(list(.Generic, .Method, .defined, .target))
sig <- c("ANY", "logical")
m0 <- selectMethod(coerce, signature = sig)
a0 <- as(0, "logical") # just `FALSE`
trace(coerce, tracer = trr, signature = sig)
m1 <- selectMethod(coerce, signature = sig)
a1 <- as(0, "logical") # error  "object '.Generic' not found"  in R <= 4.4.3
untrace(coerce, signature = sig)
m2 <- selectMethod(coerce, signature = sig)
stopifnot( is(m0, "MethodDefinition"),
          !is(m0, "MethodDefinitionWithTrace"),
           is(m1, "MethodDefinitionWithTrace"),
          identical(m0, m2), identical(a0, a1))

## Checking that "simple" as() still works:
setClass("A", slots = c(x = "NULL"))
setClass("B", slots = c(x = "NULL"))
setIs("A", "B",
      test = function(.) { TRUE },
      coerce = function(.) new("B"),
      replace = function(., value) new("B"))
B <- as(new("A"), "B") ## gave  Error in asMethod@generic :  ... `@` applied to ... "function"
stopifnot(identical(B, new("B")))


## toeplitz() implicit generic
x <- c(-1, 0,0)
r <- c(-1,11,0)
(T3 <- toeplitz(x, r))
## dummy method triggering (implicit) creation of S4 generic and default
setMethod("toeplitz", "A", function(x, ...) x)
 (mm <- selectMethod(toeplitz, "numeric"))
stopifnot(identical(T3, print(toeplitz(x, r))), removeGeneric("toeplitz"))
## badly failed since r82364 when stats::toeplitz was generalized to 3 args

## trace() a function whose S3 class() has multiple strings,
## e.g. S7 generics and methods
setOldClass(c("myfun", "function"))
f <- structure(function(x) x, class = c("myfun", "function"))
n <- 0
suppressMessages( # error: 'length = 2' in coercion to 'logical(1)'  in R <= 4.5.x
    trace("f", quote(n <<- n + 1), print = FALSE))
f1 <- f(1)
untrace("f")
stopifnot(identical(f1, 1), identical(n, 1),
          identical(class(f), c("myfun", "function")),
          identical(f(2), 2), identical(n, 1)) # f no longer traced

setClass("myS4Fun", contains = "function", slots = c(label = "character"))
g <- new("myS4Fun", function(x) x, label = "kept")
n <- 0
suppressMessages(
    trace("g", quote(n <<- n + 1), print = FALSE))
g1 <- g(1)
stopifnot(identical(g@label, "kept"))
untrace("g")
stopifnot(identical(g1, 1), identical(n, 1),
          is(g, "myS4Fun"), identical(g@label, "kept"),
          identical(g(2), 2), identical(n, 1)) # g no longer traced

setClass("myS3FunS4", contains = c("function", "VIRTUAL"),
         slots = c(label = "character"))
setOldClass(c("myS3Fun", "function"), S4Class = "myS3FunS4")
h <- structure(function(x) x, class = c("myS3Fun", "function"),
               label = "kept")
n <- 0
suppressMessages(
    trace("h", quote(n <<- n + 1), print = FALSE))
h1 <- h(1)
stopifnot(identical(h@label, "kept"))
untrace("h")
stopifnot(identical(h1, 1), identical(n, 1),
          identical(class(h), c("myS3Fun", "function")),
          identical(attr(h, "label"), "kept"),
          identical(h(2), 2), identical(n, 1)) # h no longer traced

cat('Time elapsed: ', proc.time(),'\n')
