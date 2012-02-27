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

### Find inherited group methods:
stopifnot(require(Matrix))
sm <- selectMethod("-", c("dgCMatrix", "numeric"))# direct match with "Arith"
s2 <- selectMethod("-", c("dtCMatrix", "numeric"))# ambiguity match with "Arith"
stopifnot(sm@generic == "Arith",
          s2@generic == "Arith")
## was not ok in R 2.14.x

