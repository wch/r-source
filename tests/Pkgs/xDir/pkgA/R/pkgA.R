setClass("classA", contains = "matrix")

## just so we can export it --
## *and* ensure it is "attached to graphics::plot generic" :
setMethod("plot", "classA", function(x, y, ...) NULL)

nil <- NULL # see if we can use '::' on it

## Export, so we get a *conflict* message on attaching:
search <- function(...) base::search(...)
