setClass("classA", contains = "matrix")

## just so we can export it --
## *and* ensure it is "attached to graphics::plot generic" :
setMethod("plot", "classA", function(x, y, ...) NULL)
