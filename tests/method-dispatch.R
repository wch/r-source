#### Testing  UseMethod() and even more NextMethod()
####

###-- Group methods

### Arithmetic "Ops" :
">.bar" <- function(...) print(">.bar")
">.foo" <- function(...) print(">.foo")
Ops.foo <- function(...) {
    print("Ops.foo")
    NextMethod()
}
Ops.bar <- function(...)
    print("Ops.bar")

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
    if (is.null(oldClass(x))) oldClass(x) <- data.class(x)
    cat("abc: Before dispatching; x="); str(x)
    UseMethod("abc", x,...) ## UseMethod("abc") (as in S) fails
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
