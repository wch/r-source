### This file has two purposes:

# 1) to provide a check that these errors/warnings get a sensible context.
# 2) to allow translators to see their work in action.

### Initially it is concentrating on primitives.
### There are error messages that cannot nowadays be invoked or
### could only be invoked by calling .Internals directly.

options(error=expression())

## arithmetic.c
`+`(1,2,3)
pi + "foo"
matrix(1:6,2,3) + matrix(1:6,3,2)
!"foo"
`/`(1L)
`/`(pi)
`/`(pi+1i)
sin("foo")
trunc(pi+1i)
atan(2,3)
round(pi, integer(0))
log(pi, integer(0))
log(pi, 10, 1)
1:3+1:4
1e9L*1e9L

## array.c
matrix(1:6,2,3, byrow="foo")
matrix(1:6,NA,3)
matrix(1:6,2,NA)
matrix(1:6,-1,3)
matrix(1:6,2,-1)
matrix(NA_real_, 2^17, 2^16)
row(1)
"foo" %*% pi
aperm(pi)
aperm(matrix(1:6,3.2), 3:1)
aperm(matrix(1:6,3.2), 3:2)
aperm(matrix(1:6,3.2), rep(1,1))
colSums(as.matrix(letters))
colSums(matrix(1:6,3.2), na.rm = NA)

## attrib.c
attr(NULL, "foo") <- pi
attr(pi, "tsp") <- 1
x <- numeric(0)
attr(x, "tsp") <- 1:3
comment(x) <- pi
oldClass(pi) <- "factor"
dimnames(pi) <- 1:3
A <- matrix(1:6, 2, 3)
dimnames(A) <- list(letters)
dimnames(A) <- list(letters, NULL)
dim(A) <- pi
dim(A) <- character(0)
dim(A) <- y ~ x
attr(A, 1, 2, 3)
attr(A, pi)
attr(A, letters)
attr(A, pi) <- pi
attr(A, NA_character_) <- pi

## bind.c
unlist(y ~ x)
c(pi, recursive=TRUE, recursive=FALSE)
c(list(), use.names=FALSE, use.names=TRUE)
cbind(expression(pi), pi)
cbind(1:3, 1:4)
rbind(1:3, 1:4)
cbind(matrix(1:6,2,3), matrix(1:6,3,2))
rbind(matrix(1:6,2,3), matrix(1:6,3,2))

## builtin.c
cat(letters, fill = -3)
cat(letters, sep=pi)
cat(letters, fill=3, labels=1:10)
cat(letters, append=NA)
cat(y ~ x)
vector(character(0), 0)
vector("language", 0)
a <- y ~ x
length(a) <- 5
x <- pi
length(x) <- 1:3
length(x) <- NA
switch(1:3)
delayedAssign(pi, "foo")
on.exit(ls(), add=NA_real_)
on.exit(ls(), add=NA)
on.exit(1,2,3)
x <- new.env()
parent.env(x) <- emptyenv()
parent.env(x) <- pi
parent.env(pi) <- pi

## character.c
nchar(letters, type="")
nchar(letters, type=pi)
substr("foo", integer(0), 1)
x <- pi
substr(x, integer(0), 1) <- pi
x <- "foo"
substr(x, integer(0), 1) <- pi
substr(x, 1, 1) <- pi
unlist(strsplit("a.b.c", "[.", perl = TRUE))
make.names("pi", allow_ = NA)
grep(character(0), letters)
grep("[.", letters)
grep("[.", letters, perl = TRUE)
sub("ab", "\\1", "abc")
sub("", "aa", "abc", fixed=TRUE)
x <- "MiXeD cAsE 123"
chartr("c-aX", "D-Fw", x)
chartr(NA_character_, "D-Fw", x)
chartr("ab", "c", x)
charToRaw(pi)
charToRaw(letters)
rawToChar(pi)
rawToChar(as.raw(10), multiple=NA)
rawShift(pi, 1)
rawShift(as.raw(10), -20)
rawToBits(pi)
intToBits(pi)
strtrim(paste(letters, collapse="+"), width = -10)

## coerce.c
as.vector(pi, pi)
as.function(pi)
as.function(list(), NULL)
as.function(list(), pi)
as.function(list(a=1, ls))
as.call(NULL)
as.call(expression())
is.na(y ~ x)
is.nan(y ~ x)
call(ls)
do.call("ls", pi)
do.call(y~x, list())
do.call("ls", list(), envir=pi)
substitute(2+4, pi)
x <- pi
storage.mode(x) <- pi
storage.mode(x) <- "real"
storage.mode(x) <- "single"
storage.mode(factor(letters)) <- "double"
as.raw(1777)
as.integer(baseenv())
as.integer(pi+1i)

## complex.c
gamma(1+1i)
complex(-1)
polyroot(1:50)
polyroot(c(1,2,NA))

## cum.c
cummin(1+1i)
cummax(1+1i)

## debug.c
debug(is.na)
undebug(ls)
trace(y ~ x)
tracemem(ls)
tracemem(NULL)
tracemem(baseenv())
untracemem(ls)
retracemem()
retracemem(ls)
retracemem(pi, 1, 2)
retracemem(pi, pi)

## envir.c
as.environment(NULL)
as.environment(y ~ x)
as.environment("foo")
assign(pi, pi)
assign("pi", pi)
assign("pi", pi, envir=list())
assign("pi", pi, inherits=NA_real_)
remove("x", envir=list())
remove("x", inherits=NA_real_)
remove("xxx")
get(pi)
get("")
get("pi", envir=list())
get("pi", inherits=NA_real_)
get("pi", mode=pi)
get("pi", mode="foo")
get("xxx", mode="any")
mget(pi)
mget(letters, envir=list())
mget(letters, baseenv(), inherits=NA)
mget("pi", baseenv(), mode=pi)
mget("pi", baseenv(), mode="foo")
missing(3)
attach(list(), pos="foo")
attach(list(), name=pi)
attach(list(pi))
attach(pi)
detach("package:base")
detach(pi)
ls(envir = y ~ x)
pos.to.env(integer(0))
pos.to.env(0)
as.list.environment(pi)

## eval.c
if(rep(TRUE, 10)) "foo"
f <- function() return(1,2,3)
x <- f()
f <- function() return(1,,3)
x <- f()

## main.c
q(pi)
q("foo")

## names.c
.Primitive(pi)
.Primitive("foo")
.Internal(pi)
.Internal(pairlist(list))

## objects.c
UseMethod()
f <- function(x) UseMethod(); f(pi)
f <- function(x) UseMethod(ls); f(pi)
f <- function(x) UseMethod("coef"); f(list(coefficients=pi))
f <- function(x) UseMethod("coef", x); f(list(coefficients=pi))
f <- function(x) UseMethod("coef", x, pi); f(list(coefficients=pi))
f <- function(x) UseMethod("cc"); f(list(coefficients=pi))
unclass(baseenv())
inherits(pi, pi)
inherits(pi, "factor", pi)
standardGeneric(pi)

## random.c
runif(-1, 0, 1)
sample(10, replace=logical(0))
sample(10, replace=NA)
sample(1:10, -1)
sample(1:10, 20, replace=FALSE)
sample(1:10, 3, prob=rep(0.2,5))
rmultinom(-1, 1, rep(0.2, 5))
rmultinom(1, -1, rep(0.2, 5))

## seq.c
factor(1:3) : factor(1:4)
1:1e20
x <- 2:3
x:1
1:x
1:NA
rep.int(pi, -1)
rep.int(c(pi,pi), c(-1,-2))
rep.int(y ~ x, 2)
rep.int(2, y ~ x)
rep.int(1:3, 1:2)
rep(pi, length.out = -1)
rep(pi, each = -1)
rep(pi, times=NA)
seq.int(1, length.out=-3)
seq.int(1, length.out=NA)
seq.int(Inf, 1, 2)
seq.int(1, Inf, 2)
seq.int(1, 2, NA)
seq.int(1.2, 1, by=1)
seq.int(1, 2, 3, 4, 5)
seq_len(-1)

## util.c
# arity checks
sin(1,2)
.Internal(unique(pi))
setwd(pi)
setwd("/non-existent")
basename(pi)
dirname(pi)
encodeString(pi, -1)
encodeString(pi, 10, quote=pi)
encodeString(pi, 10, quote="abc")
encodeString(pi, 10, na.encode=NA)
Encoding(pi)
Encoding(pi) <- pi
x <- "foo"
Encoding(x) <- pi
Encoding(x) <- character(0)
