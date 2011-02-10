## reset inherited methods of group members
## (contributed by Martin Morgan, 2011-2-9)
setClass("A", representation("numeric"))
a <- new("A")

setMethod("Logic", c("A", "A"), function(e1, e2) FALSE)
res0 <- a & a                           # inherit &,A,A-method
setMethod("Logic", c("A", "A"), function(e1, e2) TRUE)
stopifnot(a & a)

removeMethod("Logic", c("A", "A"))
stopifnot(logical() == a & a)

removeClass("A")
