setClass("x4_myClass", contains = "matrix")

setMethod("show", "x4_myClass",
	  function (object)
      {
	  cat("show(<myCl>): ")
	  show(object) })

setMethod("summary", "x4_myClass",
	  function (object, ...) {
	      cat("summ.(<myCl>):")
	      callNextMethod()
	  })

## Hmm, and then see what summary methods you get if you load a package which loads Matrix
## 'mgcv' is a good example, but we need one in our "base" or tests/Pkgs/

