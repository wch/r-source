##--- S4 Methods (and Classes)
library(methods)
showMethods(where = "package:methods")

## print/show dispatch  [moved from  ./reg-tests-2.R ]
setClass("bar", representation(a="numeric"))
foo <- new("bar", a=pi)
foo
show(foo)
print(foo)

setMethod("show", "bar", function(object){cat("show method\n")})
show(foo)
foo
print(foo)
print(foo, digits = 4)

print.bar <- function(x, ...) cat("print method\n")
foo
print(foo)
show(foo)

setMethod("print", "bar", function(x, ...){cat("S4 print method\n")})
foo
print(foo)
show(foo)
print(foo, digits = 4)

setClassUnion("integer or NULL", members = c("integer","NULL"))
setClass("c1", representation(x = "integer", code = "integer or NULL"))
nc <- new("c1", x = 1:2)
str(nc)# gave ^ANULL^A in 2.0.0
##


library(stats4)
showMethods(where = "package:stats4")
showMethods("show")
showMethods("show")
showMethods("plot") # (ANY,ANY) and (profile.mle, missing)
showMethods(classes="mle")
showMethods(classes="matrix")
showMethods(classes=c("matrix", "numeric"))
showMethods(where = "package:methods")

## stopifnot(require(Matrix),
##           require(lme4)) # -> S4  plot
## showMethods("plot") # more than last time
## showMethods("show", classes = c("dgeMatrix","Matrix","matrix"))
## showMethods("show")
## showMethods(classes = c("dgeMatrix","matrix"))

##--- "[" fiasco before R 2.2.0 :
d2 <- data.frame(b= I(matrix(1:6,3,2)))
## all is well:
d2[2,]
stopifnot(identical(d2[-1,], d2[2:3,]))
## Now make "[" into S4 generic by defining a trivial method
setClass("Mat", representation(Dim = "integer", "VIRTUAL"))
setMethod("[", signature(x = "Mat",
			 i = "missing", j = "missing", drop = "ANY"),
	  function (x, i, j, drop) x)
## Can even remove the method: it doesn't help
removeMethod("[", signature(x = "Mat",
                            i = "missing", j = "missing", drop = "ANY"))
d2[1:2,] ## used to fail badly; now okay
stopifnot(identical(d2[-1,], d2[2:3,]))
## failed in R <= 2.1.x
