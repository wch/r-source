#### Testing  UseMethod() and even more NextMethod()
#### --------------------
#### i.e.,  S3 methods *only*. For S4, see  reg-S4.R
##                                          ~~~~~~~~

###-- Group methods

## previous versions used print() and hit an auto-printing bug.

### Arithmetic "Ops" :
">.bar" <- function(...) {cat("using >.bar\n"); FALSE}
">.foo" <- function(...) {cat("using >.foo\n"); TRUE}
Ops.foo <- function(...) {
    cat("using Ops.foo\n")
    NextMethod()
}
Ops.bar <- function(...) {
    cat("using Ops.bar\n")
    TRUE
}

x <- 2:4 ; class(x) <- c("foo", "bar")
y <- 4:2 ; class(y) <- c("bar", "foo")

## The next 4 give a warning each about incompatible methods:
x > y
y < x # should be the same (warning msg not, however)
x == y
x <= y

x > 3 ##[1] ">.foo"

rm(list=">.foo")
x > 3 #-> "Ops.foo" and ">.bar"



### ------------  was ./mode-methods.R till R ver. 1.0.x ----------------

###-- Using Method Dispatch on "mode" etc :
## Tests S3 dispatch with the class attr forced to be data.class
## Not very relevant when S4 methods are around, but kept for historical interest
abc <- function(x, ...) {
    cat("abc: Before dispatching; x has class `", class(x), "':", sep="")
    str(x)
    UseMethod("abc", x) ## UseMethod("abc") (as in S) fails
}

abc.default <- function(x, ...) sys.call()

"abc.(" <- function(x)
    cat("'(' method of abc:", deparse(sys.call(sys.parent())),"\n")
abc.expression <- function(x)
    cat("'expression' method of abc:", deparse(sys.call(sys.parent())),"\n")

abc(1)
e0 <- expression((x))
e1 <- expression(sin(x))
abc(e0)
abc(e1)
abc(e0[[1]])
abc(e1[[1]])


## Some tests for `nameOfClass()`, called from inherits()
ClassX <- structure(list(), name = "ClassX",
                    class = c("S3pp_class", "S3pp_object"))

classx_instance <- structure(list(), class = c("ClassX", "S3pp_object"))

nameOfClass.S3pp_class <- function(x) attr(x, "name", TRUE)
nameOfClass.foo <- function(x) "bar"

stopifnot(exprs = {
    inherits(classx_instance, "ClassX")
    inherits(classx_instance, ClassX)
    ## ignore class on a character object
    isTRUE(inherits(1, structure("numeric", class = "foo")))
    ## make sure class is nor evaluated in calling nameOfClass
    isFALSE(inherits(1, structure(quote(stop("should not be evaluated")),
                                  class = "foo")))
})


## Some tests for `@` dispatching
## make sure that
## - `@` evals the first args only once,
## -  doesn't dispatch for S4
## -  works on `.Data` even for nonS4 objects

x <- structure(list(), class = "foo", prop1 = 'prop1val')
registerS3method("@", "foo",
    function(x, name) {
        stopifnot(typeof(name) == "character", length(name) == 1L)
        cat(sprintf("called `@.foo`(x = %s, name = '%s')\n",
                     deparse1(substitute(x), "\n"), name))
        attr(x, name, TRUE)
    }
)
x@prop1

abc <- x
abc@prop1

{
    cat("new x\n")
    structure(list(), class = "foo", prop1 = 'prop1val')
}@prop1

makeActiveBinding("ax", function(x) {
    cat("evaluating ax\n")
    get("x", envir = parent.frame())
}, environment())

ax@prop1

stopifnot(exprs = {
    identical( x@prop1, "prop1val")
    identical(ax@prop1, "prop1val")

    identical(letters@.Data, letters)
})

try(letters@foo) # error

# doesn't dispatch for S4
setClass("Person",
  slots = c(
    name = "character",
    age = "numeric"
  )
)

`@.Person` <- function(x, name) {
  stop("called @.Person()\n")
}

p <- new("Person", name = "Who", age = -1)
stopifnot(p@name == "Who")


## Some tests for `chooseOpsMethod()`, called from C DispatchGroup() when
## 2 methods are found
foo_obj <- structure(1, class = "foo")
bar_obj <- structure(1, class = "bar")

`+.foo` <- function(e1, e2) "foo"
`+.bar` <- function(e1, e2) "bar"

invisible(foo_obj + bar_obj)  # Warning: Incompatible methods

chooseOpsMethod.bar <- function(x, y, mx, my, cl, reverse) TRUE

stopifnot(exprs = {
    identical(foo_obj + bar_obj, "bar")
    identical(bar_obj + foo_obj, "bar")
})
