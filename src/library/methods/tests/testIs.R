## test (non-conditional) explicit inheritance
setClass("xy", representation(x="numeric", y="numeric"))

setIs("xy", "complex",
      coerce = function(from) complex(real = from@x, imaginary = from@y),
      replace = function(from, value) {
          from@x <- Re(value)
          from@y <- Im(value)
          from
      })

set.seed(124)
x1 <- rnorm(10)
y1 <- rnorm(10)
cc <- complex(real = x1, imaginary=y1)
xyc <- new("xy", x = x1, y = y1)
stopifnot(identical(cc, as(xyc, "complex")))
as(xyc, "complex") <- cc * 1i
stopifnot(identical(xyc, new("xy", x = -y1, y = x1)))

setGeneric("size", function(x)standardGeneric("size"))
## check that generic for size() was created w/o a default method
stopifnot(is(size, "standardGeneric"),
          is.null(selectMethod("size", "ANY",optional=TRUE)))

setMethod("size", "vector", function(x)length(x))

## class "xy" should inherit the vector method through complex
stopifnot(identical(size(xyc), length(x1)))
removeClass("xy")
removeGeneric("size")
