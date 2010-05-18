setClass("maybe")

setClass("A", representation(x = "numeric"))

setIs("A", "maybe", test = function(object)length(object@x) >= 1 && object@x[[1]]>0,
      coerce = function(from)from,
      replace = function(from, value) stop("meaningless to replace the \"maybe\" part of an object"))

aa = new("A", x=1)

ff <- function(x)"default ff"

setGeneric("ff")

ffMaybe <- function(x) "ff maybe method"

setMethod("ff", "maybe", ffMaybe)

stopifnot(identical(ff(aa), "default ff"))

aa2 <- new("A", x = -1) # condition not TRUE

stopifnot(identical(ff(aa2), "default ff"))

## a method to test the condition
setMethod("ff", "A",
          function(x) {
              if(is(x, "maybe"))
                  ffMaybe(x)
              else
                callNextMethod()
          })
stopifnot(identical(ff(aa), "ff maybe method"))
stopifnot(identical(ff(aa2), "default ff"))

removeClass("A")
removeClass("maybe")
removeGeneric("ff")
