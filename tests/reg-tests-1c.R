## Regression tests for R >= 3.0.0

pdf("reg-tests-1c.pdf", encoding = "ISOLatin1.enc")

## mapply with classed objects with length method
## was not documented to work in 2.x.y
setClass("A", representation(aa = "integer"))
a <- new("A", aa = 101:106)
setMethod("length", "A", function(x) length(x@aa))
setMethod("[[", "A", function(x, i, j, ...) x@aa[[i]])
(z <- mapply(function(x, y) {x * y}, a, rep(1:3, 2)))
stopifnot(z == c(101, 204, 309, 104, 210, 318))
## reported as a bug (which it was not) by H. Pages in
## https://stat.ethz.ch/pipermail/r-devel/2012-November/065229.html

## recyling in split()
## https://stat.ethz.ch/pipermail/r-devel/2013-January/065700.html
x <- 1:6
y <- split(x, 1:2)
class(x) <- "ABC" ## class(x) <- "A" creates an invalid object
yy <- split(x, 1:2)
stopifnot(identical(y, yy))
## were different in R < 3.0.0


## dates with fractional seconds after 2038 (PR#15200)
## Extremely speculative!
z <- as.POSIXct(2^31+c(0.4, 0.8), origin=ISOdatetime(1970,1,1,0,0,0,tz="GMT"))
zz <- format(z)
stopifnot(zz[1] == zz[2])
## printed form rounded not truncated in R < 3.0.0

## origin coerced in tz and not GMT by as.POSIXct.numeric()
x <- as.POSIXct(1262304000, origin="1970-01-01", tz="EST")
y <- as.POSIXct(1262304000, origin=.POSIXct(0, "GMT"), tz="EST")
stopifnot(identical(x, y))

## Handling records with quotes in names
x <- c("a b' c",
"'d e' f g",
"h i 'j",
"k l m'")
y <- data.frame(V1 = c("a", "d e", "h"), V2 = c("b'", "f", "i"), V3 = c("c", "g", "j\nk l m"))
f <- tempfile()
writeLines(x, f)
stopifnot(identical(count.fields(f), c(3L, 3L, NA_integer_, 3L)))
stopifnot(identical(read.table(f), y))
stopifnot(identical(scan(f, ""), as.character(t(as.matrix(y)))))

## docu always said  'length 1 is sorted':
stopifnot(!is.unsorted(NA))

## str(.) for large factors should be fast:
u <- as.character(runif(1e5))
t1 <- max(0.001, system.time(str(u))[[1]]) # get a baseline > 0
uf <- factor(u)
(t2 <- system.time(str(uf))[[1]]) / t1 # typically around 1--2
stopifnot(t2  / t1 < 30)
## was around 600--850 for R <= 3.0.1


## ftable(<array with unusual dimnames>)
(m <- matrix(1:12, 3,4, dimnames=list(ROWS=paste0("row",1:3), COLS=NULL)))
ftable(m)
## failed to format (and hence print) because of NULL 'COLS' dimnames

## regression test formerly in kmeans.Rd, but result differs by platform
## Artificial example [was "infinite loop" on x86_64; PR#15364]
rr <- c(rep(-0.4, 5), rep(-0.4- 1.11e-16, 14), -.5)
r. <- signif(rr, 12)
k3 <- kmeans(rr, 3, trace=2) ## Warning: Quick-Transfer.. steps exceed
try ( k. <- kmeans(r., 3) ) # after rounding, have only two distinct points
      k. <- kmeans(r., 2)   # fine


## PR#15376
stem(c(1, Inf))
## hung in 3.0.1


## PR#15377, very long variable names
x <- 1:10
y <- x + rnorm(10)
z <- y + rnorm(10)
yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy <- y
fit <- lm(cbind(yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy, z) ~ x)
## gave spurious error message in 3.0.1.

## PR#15341 singular complex matrix in rcond()
set.seed(11)
n <- 5
A <- matrix(runif(n*n),nrow=n)
B <- matrix(runif(n*n),nrow=n)
B[n,] <- (B[n-1,]+B[n-2,])/2
rcond(B)
B <- B + 0i
rcond(B)
## gave error message (OK) in R 3.0.1: now returns 0 as in real case.


## Misuse of formatC as in PR#15303
days <- as.Date(c("2012-02-02", "2012-03-03", "2012-05-05"))
(z <- formatC(days))
stopifnot(!is.object(z), is.null(oldClass(z)))
## used to copy over class in R < 3.0.2.


## PR15219
val <- sqrt(pi)
fun <- function(x) (-log(x))^(-1/2)
(res <- integrate(fun, 0, 1, rel.tol = 1e-4))
stopifnot(abs(res$value - val) < res$abs.error)
(res <- integrate(fun, 0, 1, rel.tol = 1e-6))
stopifnot(abs(res$value - val) < res$abs.error)
res <- integrate(fun, 0, 1, rel.tol = 1e-8)
stopifnot(abs(res$value - val) < res$abs.error)

fun <- function(x) x^(-1/2)*exp(-x)
(res <- integrate(fun, 0, Inf, rel.tol = 1e-4))
stopifnot(abs(res$value - val) < res$abs.error)
(res <- integrate(fun, 0, Inf, rel.tol = 1e-6))
stopifnot(abs(res$value - val) < res$abs.error)
(res <- integrate(fun, 0, Inf, rel.tol = 1e-8))
stopifnot(abs(res$value - val) < res$abs.error)
## sometimes exceeded reported error in 2.12.0 - 3.0.1


## Unary + should coerce
x <- c(TRUE, FALSE, NA, TRUE)
stopifnot(is.integer(+x))
## +x was logical in R <= 3.0.1


## Attritbutes of value of unary operators
# +x, -x were ts, !x was not in 3.0.2
x <- ts(c(a=TRUE, b=FALSE, c=NA, d=TRUE), frequency = 4, start = 2000)
x; +x; -x; !x
stopifnot(is.ts(!x), !is.ts(+x), !is.ts(-x))
# +x, -x were ts, !x was not in 3.0.2
x <- ts(c(a=1, b=2, c=0, d=4), frequency = 4, start = 2010)
x; +x; -x; !x
stopifnot(!is.ts(!x), is.ts(+x), is.ts(-x))
##


## regression test incorrectly in colorRamp.Rd
bb <- colorRampPalette(2)(4)
stopifnot(bb[1] == bb)
## special case, invalid in R <= 2.15.0:


## Setting NAMED on ... arguments
f <- function(...) { x <- (...); x[1] <- 7; (...) }
stopifnot(f(1+2) == 3)
## was 7 in 3.0.1


## copying attributes from only one arg of a binary operator.
A <- array(c(1), dim = c(1L,1L), dimnames = list("a", 1))
x <- c(a = 1)
B <- A/(pi*x)
stopifnot(is.null(names(B)))
## was wrong in R-devel in Aug 2013
## needed an un-NAMED rhs.


## lgamma(x) for very small negative x
X <- 3e-308; stopifnot(identical(lgamma(-X), lgamma(X)))
## lgamma(-X) was NaN in R <= 3.0.1


## PR#15413
z <- subset(data.frame(one = numeric()), select = one)
stopifnot(nrow(z) == 0L)
## created a row prior to 3.0.2


## https://stat.ethz.ch/pipermail/r-devel/2013-September/067524.html
dbeta(0.9, 9.9e307, 10)
dbeta(0.1, 9,  9.9e307)
dbeta(0.1, 9.9e307, 10)
## first two hung in R <= 3.0.2

## PR#15465
provideDimnames(matrix(nrow = 0, ncol = 1))
provideDimnames(table(character()))
as.data.frame(table(character()))
## all failed in 3.0.2

## PR#15004
n <- 10
s <- 3
l <- 10000
m <- 20
x <- data.frame(x1 = 1:n, x2 = 1:n)
by <- data.frame(V1 = factor(rep(1:3, n %/% s + 1)[1:n], levels = 1:s))
for(i in 1:m) {
    by[[i + 1]] <- factor(rep(l, n), levels = 1:l)
}
agg <- aggregate.data.frame(x, by, mean)
stopifnot(nrow(unique(by)) == nrow(agg))
## rounding caused groups to be falsely merged

## PR#15454
set.seed(357)
z <- matrix(c(runif(50, -1, 1), runif(50, -1e-190, 1e-190)), nrow = 10)
contour(z)
## failed because rounding made crossing tests inconsistent

## Various cases where zero length vectors were not handled properly
## by functions in base and utils, including PR#15499
y <- as.data.frame(list())
format(y)
format(I(integer()))
gl(0, 2)
z <- list(numeric(0), 1)
stopifnot(identical(relist(unlist(z), z), z))
summary(y)
## all failed in 3.0.2

## PR#15518 Parser catching errors in particular circumstance:
(ee <- tryCatch(parse(text = "_"), error= function(e)e))
stopifnot(inherits(ee, "error"))
## unexpected characters caused the parser to segfault in 3.0.2


## nonsense value of nmax
unique(1:3, nmax = 1)
## infinite-looped in 3.0.2, now ignored.


## besselI() (and others), now using sinpi() etc:
stopifnot(all.equal(besselI(2.125,-5+1/1024),
		    0.02679209380095711, tol= 8e-16),
	  all.equal(lgamma(-12+1/1024), -13.053274367453049, tol=8e-16))
## rel.error was 1.5e-13 / 7.5e-14 in R <= 3.0.x
ss <- sinpi(2*(-10:10)-2^-12)
tt <- tanpi(  (-10:10)-2^-12)
stopifnot(ss == ss[1], tt == tt[1], # as internal arithmetic must be exact here
	  all.equal(ss[1], -0.00076699031874270453, tol=8e-16),
	  all.equal(tt[1], -0.00076699054434309260, tol=8e-16))
## (checked via Rmpfr) The above failed during development


## PR#15535 c() "promoted" raw vectors to bad logical values
stopifnot( c(as.raw(11), TRUE) == TRUE )
## as.raw(11) became a logical value coded as 11,
## and did not test equal to TRUE.


## PR#15564
fit <- lm(rnorm(10) ~ I(1:10))
predict(fit, interval = "confidence", scale = 1)
## failed in <= 3.0.2 with object 'w' not found


## PR#15534 deparse() did not produce reparseable complex vectors
assert.reparsable <- function(sexp) {
  deparsed <- paste(deparse(sexp), collapse=" ")
  reparsed <- tryCatch(eval(parse(text=deparsed)[[1]]), error = function(e) NULL)
  if (is.null(reparsed))
    stop(sprintf("Deparsing produced invalid syntax: %s", deparsed))
  if(!identical(reparsed, sexp))
    stop(sprintf("Deparsing produced change: value is not %s", reparsed))
}

assert.reparsable(1)
assert.reparsable("string")
assert.reparsable(2+3i)
assert.reparsable(1:10)
assert.reparsable(c(NA, 12, NA, 14))
assert.reparsable(as.complex(NA))
assert.reparsable(complex(real=Inf, i=4))
assert.reparsable(complex(real=Inf, i=Inf))
assert.reparsable(complex(real=Inf, i=-Inf))
assert.reparsable(complex(real=3, i=-Inf))
assert.reparsable(complex(real=3, i=NaN))
assert.reparsable(complex(r=NaN, i=0))
assert.reparsable(complex(real=NA, i=1))
assert.reparsable(complex(real=1, i=NA))
## last 7 all failed


## PR#15621 backticks could not be escaped
stopifnot(deparse(as.name("`"), backtick=TRUE) == "`\\``")
assign("`", TRUE)
`\``
tools::assertError(parse("```"))
##


## We document tanpi(0.5) etc to be NaN
stopifnot(is.nan(tanpi(c(0.5, 1.5, -0.5, -1.5))))
## That is not required for system implementations, and some give +/-Inf


## PR#15642 segfault when parsing overflowing reals
as.double("1e1000")


ll <- ml <- list(1,2); dim(ml) <- 2:1
ali <- all.equal(list( ), identity)  # failed in R-devel for ~ 30 hours
al1 <- all.equal(list(1), identity)  # failed in R < 3.1.0
stopifnot(length(ali) == 3, grepl("list", ali[1]),
	  grepl("length", ali[2], ignore.case=TRUE),
	  is.character(al1), length(al1) >= 2,
	  all.equal(ml, ml),
	  all.equal(ll, ml, check.attributes=FALSE))


## PR#15699 aggregate failed when there were no grouping variables
dat <- data.frame(Y = runif(10), X = sample(LETTERS[1:3], 10, TRUE))
aggregate(Y ~ 1, FUN = mean, data = dat)


## merge() with duplicated column names, similar to PR#15618
X <- data.frame(Date = c("1967-02-01", "1967-02-02", "1967-02-03"),
                Settle.x = c(NA, NA, NA), Settle.y = c(NA, NA, NA),
                Settle = c(35.4, 35.15, 34.95))
Y <- data.frame(Date = c("2013-12-10", "2013-12-11", "2013-12-12"),
                Settle = c(16.44, 16.65, 16.77))
merge(X, Y, by = "Date", all = TRUE)
## failed in R < 3.1.0: now warns (correctly).


## PR#15679
badstructure <- function(depth, key)
{
    ch <- if (depth == 1L) list() else list(badstructure(depth-1,key))
    r <- list()
    r[[key]] <- ch
    r
}
badstructure(20, "children")
## overran, segfaulted for the original reporter.


## PR#15702 and PR#15703
d <- as.dendrogram(hclust(dist(sin(1:7))))
(dl <- d[[c(2,1,2)]]) # single-leaf dendrogram
stopifnot(inherits(dl, "dendrogram"), is.leaf(dl),
	  identical(order.dendrogram(dl), as.vector(dl)),
	  identical(d, as.dendrogram(d)))
## as.dendrogram() was hidden;  order.*() failed for leaf


## using *named* method
hw <- hclust(dist(sqrt(1:5)), method=c(M = "ward"))
## failed for 2 days in R-devel/-alpha


## PR#15758
my_env <- new.env(); my_env$one <- 1L
save(one, file = tempfile(), envir = my_env)
## failed in R < 3.1.1.


## Conversion to numeric in boundary case
ch <- "0x1.ffa0000000001p-1"
rr <- type.convert(ch, numerals = "allow.loss")
rX <- type.convert(ch, numerals = "no.loss")
stopifnot(is.numeric(rr), identical(rr, rX),
          all.equal(rr, 0.999267578125),
	  all.equal(type.convert(ch,	      numerals = "warn"),
		    type.convert("0x1.ffap-1",numerals = "warn"), tol = 5e-15))
## type.convert(ch) was not numeric in R 3.1.0
##
ch <- "1234567890123456789"
rr <- type.convert(ch, numerals = "allow.loss")
rX <- type.convert(ch, numerals = "no.loss")
rx <- type.convert(ch, numerals = "no.loss", as.is = TRUE)
tools::assertWarning(r. <- type.convert(ch, numerals = "warn.loss"))
stopifnot(is.numeric(rr), identical(rr, r.), all.equal(rr, 1.234567890e18),
	  is.factor(rX),  identical(rx, ch))


## PR#15764: integer overflow could happen without a warning or giving NA
tools::assertWarning(ii <- 1980000020L + 222000000L)
stopifnot(is.na(ii))
tools::assertWarning(ii <- (-1980000020L) + (-222000000L))
stopifnot(is.na(ii))
tools::assertWarning(ii <- (-1980000020L) - 222000000L)
stopifnot(is.na(ii))
tools::assertWarning(ii <- 1980000020L - (-222000000L))
stopifnot(is.na(ii))
## first two failed for some version of clang in R < 3.1.1


## PR#15735: formulae with exactly 32 variables
myFormula <- as.formula(paste(c("y ~ x0", paste0("x", 1:30)), collapse = "+"))
ans <- update(myFormula, . ~ . - w1)
stopifnot(identical(ans, myFormula))

updateArgument <-
    as.formula(paste(c(". ~ . ", paste0("w", 1:30)), collapse = " - "))
ans2 <- update(myFormula, updateArgument)
stopifnot(identical(ans2, myFormula))


## PR#15753
0x110p-5L
stopifnot(.Last.value == 8.5)
## was 272 with a garbled message in R 3.0.0 - 3.1.0.


## numericDeriv failed to duplicate variables in
## the expression before modifying them.  PR#15849
x <- 10; y <- 10
d1 <- numericDeriv(quote(x+y),c("x","y"))
x <- y <- 10
d2 <- numericDeriv(quote(x+y),c("x","y"))
stopifnot(identical(d1,d2))
## The second gave the wrong answer


## prettyNum(x, zero.print = .) failed when x had NAs
pp <- sapply(list(TRUE, FALSE, ".", " "), function(.)
	     prettyNum(c(0:1,NA), zero.print = . ))
stopifnot(identical(pp[1,], c("0", " ", ".", " ")),
	  pp[2:3,] == c("1","NA"))
## all 4 prettyNum() would error out


## checking all.equal() with externalptr
library(methods) # getClass()'s versionKey is an e.ptr
cA <- getClass("ANY")
stopifnot(all.equal(cA, cA),
          is.character(all.equal(cA, getClass("S4"))))
# both all.equal() failed in R <= 3.1.1


## as.hexmode(x), as.octmode(x)  when x is double
x <- c(NA, 1)
stopifnot(identical(x == x,
		    as.hexmode(x) == as.octmode(x)))
p <- c(1, pi)
tools::assertError(as.hexmode(p))
tools::assertError(as.octmode(p))
## where all "wrong" in R <= 3.1.1


## PR#15935
y <- 1:3
drop1(lm(y ~ 1))
drop1(glm(y ~ 1))
stats:::drop1.default(glm(y ~ 1))
## gave error in R < 3.1.2

## getAnywhere() wrongly dealing with namespace hidden list object
nm <- deparse(body(pbinom)[[2]])# == "C_pbinom" currently
gg <- getAnywhere(nm)
stopifnot(length(gg$objs) == 1)
## was 4 and printed "4 differing objects matching ‘C_pbinom’ ..." in R <= 3.1.1


## 0-length consistency of options(), PR#15979
stopifnot(identical(options(list()), options(NULL)))
## options(list()) failed in R <= 3.1.1


## merge.dendrogram(), PR#15648
mkDend <- function(n, lab, rGen = function(n) 1+round(16*abs(rnorm(n)))) {
    stopifnot(is.numeric(n), length(n) == 1, n >= 1, is.character(lab))
    a <- matrix(rGen(n*n), n, n)
    colnames(a) <- rownames(a) <- paste0(lab, 1:n)
    as.dendrogram(hclust(as.dist(a + t(a))))
}
set.seed(7)
da <- mkDend(4, "A")
db <- mkDend(3, "B")
d.ab <- merge(da, db)
hcab <- as.hclust(d.ab)
stopifnot(hcab$order == c(2, 4, 1, 3, 7, 5, 6),
	  hcab$labels == c(paste0("A", 1:4), paste0("B", 1:3)))
## was wrong in R <= 3.1.1


## bw.SJ() and similar with NA,Inf values, PR#16024
try(bw.SJ (c(NA,2,3)))
try(bw.bcv(c(-Inf,2,3)))
try(bw.ucv(c(1,NaN,3,4)))
## seg.faulted  in  3.0.0 <= R <= 3.1.1


## as.dendrogram() with wrong input
x <- rbind(c( -6, -9), c(  0, 13),
	   c(-15,  6), c(-14,  0), c(12,-10))
dx <- dist(x,"manhattan")
hx <- hclust(dx)
hx$merge <- matrix(c(-3, 1, -2, 3,
                     -4, -5, 2, 3), 4,2)
tools::assertError(as.dendrogram(hx))
## 8 member dendrogram and memory explosion for larger examples in R <= 3.1.2


## abs with named args failed, PR#16047
abs(x=1i)
## Complained that the arg should be named z


## Big exponents overflowed, PR#15976
x <- 0E4933
y <- 0x0p100000
stopifnot(x == 0, y == 0)
##


## drop.terms() dropped some attributes, PR#16029
test <- model.frame(Employed ~ Year + poly(GNP,3) + Population, data=longley)
mterm <- terms(test)
mterm2 <- drop.terms(mterm, 3)
predvars <- attr(mterm2, "predvars")
dataClasses <- attr(mterm2, "dataClasses")
factors <- attr(mterm2, "factors")
stopifnot(is.language(predvars), length(predvars) == length(dataClasses)+1,
          all(names(dataClasses) == rownames(factors)))
## Previously dropped predvars and dataClasses


## prompt() did not escape percent signs properly
fn <- function(fmt = "%s") {}
f <- tempfile(fileext = ".Rd")
prompt(fn, filename = f)
rd <- tools::parse_Rd(f)
## Gave syntax errors because the percent sign in Usage
## was taken as the start of a comment.


## power.t.test() failure for very large n (etc): PR#15792
(ptt <- power.t.test(delta = 1e-4, sd = .35, power = .8))
(ppt <- power.prop.test(p1 = .5, p2 = .501, sig.level=.001, power=0.90, tol=1e-8))
stopifnot(all.equal(ptt$n, 192297000, tol = 1e-5),
          all.equal(ppt$n,  10451937, tol = 1e-7))
## call to uniroot() did not allow n > 1e7


## save(*, ascii=TRUE):  PR#16137
x0 <- x <- c(1, NA, NaN)
save(x, file=(sf <- tempfile()), ascii = TRUE)
load(sf)
stopifnot(identical(x0, x))
## x had 'NA' instead of 'NaN'


## PR#16205
stopifnot(length(glob2rx(character())) == 0L)
## was "^$" in R < 3.1.3


### Bugs fixed in R 3.2.0

## Bugs reported by Radford Neal
x <- pairlist(list(1, 2))
x[[c(1, 2)]] <- NULL   # wrongly gave an error, referring to misuse
                       # of the internal SET_VECTOR_ELT procedure
stopifnot(identical(x, pairlist(list(1))))

a <- pairlist(10, 20, 30, 40, 50, 60)
dim(a) <- c(2, 3)
dimnames(a) <- list(c("a", "b"), c("x", "y", "z"))
# print(a)              # doesn't print names, not fixed
a[["a", "x"]] <- 0
stopifnot(a[["a", "x"]] == 0)
## First gave a spurious error, second caused a seg.fault


## Radford (R-devel, June 24, 2014); M.Maechler
m <- matrix(1:2, 1,2); v <- 1:3
stopifnot(identical(crossprod(2, v), t(2) %*% v),
	  identical(crossprod(m, v), t(m) %*% v),
	  identical(5 %*% v, 5 %*% t(v)),
          identical(tcrossprod(m, 1:2), m %*% 1:2) )
## gave error "non-conformable arguments" in R <= 3.2.0


## list <--> environment
L0 <- list()
stopifnot(identical(L0, as.list(as.environment(L0))))
## as.env..() did not work, and as.list(..) gave non-NULL names in R 3.1.x


## all.equal() for environments and refClass()es
RR <- setRefClass("Ex", fields = list(nr = "numeric"))
m1 <- RR$new(); m2 <- RR$new(); m3 <- RR$new(nr = pi); m4 <- RR$new(nr=3.14159)
ee <- emptyenv(); e2 <- new.env()
stopifnot(all.equal(ee,ee), identical(ee,ee), !identical(ee,e2), all.equal(ee,e2),
	  identical(m3,m3), !identical(m1,m2),
	  all.equal(m1,m2), !isTRUE(all.equal(m1,m3)), !isTRUE(all.equal(m1,m4)),
	  all.equal(m3,m4, tol=1e-6), grepl("relative difference", all.equal(m3,m4)),
	  TRUE)
## did not work in R 3.1.x
e3 <- new.env()
e3$p <- "p"; e2$p <- "p"; ae.p <- all.equal(e2,e3)
e3$q <- "q";              ae.q <- all.equal(e2,e3)
e2$q <- "Q";              ae.Q <- all.equal(e2,e3)
stopifnot(ae.p, grepl("^Length", ae.q), grepl("string mismatch", ae.Q))
e2$q <- "q"; e2$r <- pi; e3$r <- 3.14159265
stopifnot(all.equal(e2, e3),
	  grepl("relative difference", all.equal(e2, e3, tol=1e-10)))
g <- globalenv() # so it now contains itself
l <- list(e = g)
stopifnot(all.equal(g, g),
	  all.equal(l, l))
## these ran into infinite recursion error.


## missing() did not propagate through '...', PR#15707
check <- function(x,y,z) c(missing(x), missing(y), missing(z))
check1 <- function(...) check(...)
check2 <- function(...) check1(...)
stopifnot(identical(check2(one, , three), c(FALSE, TRUE, FALSE)))
## missing() was unable to handle recursive promises


## envRefClass prototypes are a bit special -- broke all.equal() for baseenv()
rc <- getClass("refClass")
rp <- rc@prototype
str(rp) ## failed
rp ## show() failed ..
(ner <- new("envRefClass")) # show() failed
stopifnot(all.equal(rp,rp), all.equal(ner,ner))
be <- baseenv()
system.time(stopifnot(all.equal(be,be)))## <- takes a few sec's
stopifnot(
    grepl("not identical.*character", print(all.equal(rp, ner))),
    grepl("not identical.*character", print(all.equal(ner, rp))))
system.time(stopifnot(all.equal(globalenv(), globalenv())))
## Much of the above failed in  R <= 3.2.0


## while did not protect its argument, which caused an error
## under gctorture, PR#15990
gctorture()
suppressWarnings(while(c(FALSE, TRUE)) 1)
gctorture(FALSE)
## gave an error because the test got released when the warning was generated.


## hist(x, breaks =) with too large bins, PR#15988
set.seed(5); x <- runif(99)
Hist <- function(x, b) hist(x, breaks = b, plot = FALSE)$counts
for(k in 1:5) {
    b0 <- seq_len(k-1)/k
    H.ok <- Hist(x, c(-10, b0, 10))
    for(In in c(1000, 1e9, Inf))
	stopifnot(identical(Hist(x, c(-In, b0, In)), H.ok),
		  identical(Hist(x, c( 0,  b0, In)), H.ok))
}
## "wrong" results for k in {2,3,4} in R 3.1.x


## eigen(*, symmetric = <default>) with asymmetric dimnames,  PR#16151
m <- matrix(c(83,41), 5, 4,
	    dimnames=list(paste0("R",1:5), paste0("C",1:4)))[-5,] + 3*diag(4)
stopifnot( all.equal(eigen(m, only.values=TRUE) $ values,
		     c(251, 87, 3, 3), tol=1e-14) )
## failed, using symmetric=FALSE and complex because of the asymmetric dimnames()


## match.call() re-matching '...'
test <- function(x, ...) test2(x, 2, ...)
test2 <- function(x, ...) match.call(test2, sys.call())
stopifnot(identical(test(1, 3), quote(test2(x=x, 2, 3))))
## wrongly gave test2(x=x, 2, 2, 3) in R <= 3.1.2


## callGeneric not forwarding dots in call (PR#16141)
setGeneric("foo", function(x, ...) standardGeneric("foo"))
setMethod("foo", "character",
          function(x, capitalize = FALSE) if (capitalize) toupper(x) else x)
setMethod("foo", "factor",
          function(x, capitalize = FALSE) { x <- as.character(x);  callGeneric() })
toto1 <- function(x, ...) foo(x, ...)
stopifnot(identical(toto1(factor("a"), capitalize = TRUE), "A"))
## wrongly did not capitalize in R <= 3.1.2


## Accessing non existing objects must be an error
tools::assertError(base :: foobar)
tools::assertError(base :::foobar)
tools::assertError(stats:::foobar)
tools::assertError(stats:: foobar)
## lazy data only via '::', not ':::' :
stopifnot(    nrow(datasets:: swiss) == 47)
tools::assertError(datasets:::swiss)
## The ::: versions gave NULL in certain development versions of R
stopifnot(identical(stats4::show -> s4s,
		    get("show", asNamespace("stats4") -> ns4)),
	  s4s@package == "methods",
	  is.null(ns4[["show"]]) # not directly in stats4 ns
	  )
## stats4::show was NULL for 4 hours in R-devel


## mode<- did too much evaluation (PR#16215)
x <- y <- quote(-2^2)
x <- as.list(x)
mode(y) <- "list"
stopifnot(identical(x, y))
## y ended up containing -4, not -2^2


## besselJ()/besselY() with too large order
besselJ(1, 2^64) ## NaN with a warning
besselY(1, c(2^(60:70), Inf))
## seg.faulted in R <= 3.1.2


## besselJ()/besselY() with  nu = k + 1/2; k in {-1,-2,..}
besselJ(1, -1750.5) ## Inf, with only one warning...
stopifnot(is.finite(besselY(1, .5 - (1500 + 0:10))))
## last gave NaNs; both: more warnings in R <= 3.1.x


## BIC() for arima(), also with NA's
lho <- lh; lho[c(3,7,13,17)] <- NA
alh300 <- arima(lh,  order = c(3,0,0))
alh311 <- arima(lh,  order = c(3,1,1))
ao300  <- arima(lho, order = c(3,0,0))
ao301  <- arima(lho, order = c(3,0,1))
## AIC/BIC for *different* data rarely makes sense ... want warning:
tools::assertWarning(AA <- AIC(alh300,alh311, ao300,ao301))
tools::assertWarning(BB <- BIC(alh300,alh311, ao300,ao301))
fmLst <- list(alh300,alh311, ao300,ao301)
## nobs() did not "work" in R < 3.2.0:
stopifnot(sapply(fmLst, nobs) == c(48,47, 44,44))
lls <- lapply(fmLst, logLik)
str(lapply(lls, unclass))# -> 'df' and 'nobs'
## 'manual BIC' via generalized AIC:
stopifnot(all.equal(BB[,"BIC"],
                    sapply(fmLst, function(fm) AIC(fm, k = log(nobs(fm))))))
## BIC() was NA unnecessarily in  R < 3.2.0; nobs() was not available eiher


## as.integer() close and beyond maximal integer
MI <- .Machine$integer.max
stopifnot(identical( MI, as.integer( MI + 0.99)),
	  identical(-MI, as.integer(-MI - 0.99)),
	  is.na(as.integer(as.character( 100*MI))),
	  is.na(as.integer(as.character(-100*MI))))
## The two cases with positive numbers  failed in R <= 3.2.0


## Ensure that sort() works with a numeric vector "which is an object":
stopifnot(is.object(y <- freeny$y))
stopifnot(diff(sort(y)) > 0)
## order() and hence sort() failed here badly for a while around 2015-04-16


## NAs in data frame names:
dn <- list(c("r1", NA), c("V", NA))
d11 <- as.data.frame(matrix(c(1, 1, 1, 1), ncol = 2, dimnames = dn))
stopifnot(identical(names(d11), dn[[2]]),
          identical(row.names(d11), dn[[1]]))
## as.data.frame() failed in R-devel for a couple of hours ..
## note that format(d11) does fail currently, and hence print(), too


## Ensure  R -e ..  works on Unix
if(.Platform$OS.type == "unix" &&
   file.exists(Rc <- file.path(R.home("bin"), "R")) &&
   file.access(Rc, mode = 1) == 0) { # 1: executable
    cmd <- paste(Rc, "-q --vanilla -e 1:3")
    ans <- system(cmd, intern=TRUE)
    stopifnot(length(ans) >= 3,
	      identical(ans[1:2], c("> 1:3",
				    "[1] 1 2 3")))
}
## (failed for < 1 hr, in R-devel only)


## Parsing large exponents of floating point numbers, PR#16358
set.seed(12)
lrg <- sprintf("%.0f", round(exp(10*(2+abs(rnorm(2^10))))))
head(huge <- paste0("1e", lrg))
    micro <- paste0("1e-", lrg)
stopifnot(as.numeric(huge) == Inf,
          as.numeric(micro) == 0)
## Both failed in R <= 3.2.0


## vcov() failed on manova() results, PR#16380
tear <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3, 6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
gloss <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4, 9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
opacity <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7, 2.8, 4.1, 3.8,1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
Y <- cbind(tear, gloss, opacity)
rate <- factor(gl(2,10), labels = c("Low", "High"))
fit <- manova(Y ~ rate)
vcov(fit)
## Gave error because coef.aov() turned matrix of coefficients into a vector


## Unary / Binary uses of logic operations, PR#16385
tools::assertError(`&`(FALSE))
tools::assertError(`|`(TRUE))
## Did not give errors in R <= 3.2.0
E <- tryCatch(`!`(), error = function(e)e)
stopifnot(grepl("0 arguments .*\\<1", conditionMessage(E)))
## Gave wrong error message in R <= 3.2.0
stopifnot(identical(!matrix(TRUE), matrix(FALSE)),
	  identical(!matrix(FALSE), matrix(TRUE)))
## was wrong for while in R 3.2.0 patched


## cummax(<integer>)
iNA <- NA_integer_
x <- c(iNA, 1L)
stopifnot(identical(cummin(x), c(iNA, iNA)),
          identical(cummax(x), c(iNA, iNA)))
## an initial NA was not propagated in R <= 3.2.0


## summaryRprof failed for very short profile, PR#16395
profile <- tempfile()
writeLines(c(
'memory profiling: sample.interval=20000',
':145341:345360:13726384:0:"stdout"',
':208272:345360:19600000:0:"stdout"'), profile)
summaryRprof(filename = profile, memory = "both")
unlink(profile)
## failed when a matrix was downgraded to a vector


## option(OutDec = *)  -- now gives a warning when  not 1 character
op <- options(OutDec = ".", digits = 7, # <- default
              warn = 2)# <- (unexpected) warnings become errors
stopifnot(identical("3.141593", fpi <- format(pi)))
options(OutDec = ",")
stopifnot(identical("3,141593", cpi <- format(pi)))
## warnings, but it "works" (for now):
tools::assertWarning(options(OutDec = ".1."))
stopifnot(identical("3.1.141593", format(pi)))
tools::assertWarning(options(OutDec = ""))
tools::assertWarning(stopifnot(identical("3141593", format(pi))))
options(op)# back to sanity
## No warnings in R versions <= 3.2.1


## format(*, decimal.mark=".")  when   OutDec != "."  (PR#16411)
op <- options(OutDec = ",")
stopifnot(identical(fpi, format(pi, decimal.mark=".")))
## failed in R <= 3.2.1


## model.frame() removed ts attributes on original data (PR#16436)
orig <- class(EuStockMarkets)
mf <- model.frame(EuStockMarkets ~ 1, na.action=na.fail)
stopifnot(identical(orig, class(EuStockMarkets)))
## ts class lost in R <= 3.2.1


##
foo <- as.expression(1:3)
matrix(foo, 3, 3) # always worked
matrix(foo, 3, 3, byrow = TRUE)
## failed in R <= 3.1.2


## labels.dendrogram(), dendrapply(), etc -- see comment #15 of PR#15215 :
(D <- as.dendrogram(hclust(dist(cbind(setNames(c(0,1,4), LETTERS[1:3]))))))
stopifnot(
    identical(labels(D), c("C", "A", "B")),
    ## has been used in "CRAN package space"
    identical(suppressWarnings(dendrapply(D, labels)),
              list("C", list("A", "B"), "C")))
## dendrapply(D, labels) failed in R-devel for a day or two


## poly() / polym() predict()ion
library(datasets)
alm <- lm(stack.loss ~ poly(Air.Flow, Water.Temp, degree=3), stackloss)
f20 <- fitted(alm)[1:20] # "correct" prediction values [1:20]
stopifnot(all.equal(unname(f20[1:4]), c(39.7703378, 39.7703378, 35.8251359, 21.5661761)),
	  all.equal(f20, predict(alm, stackloss) [1:20] , tolerance = 1e-14),
	  all.equal(f20, predict(alm, stackloss[1:20, ]), tolerance = 1e-14))
## the second prediction went off in  R <= 3.2.1


## PR#16478
kkk <- c("a\tb", "3.14\tx")
z1 <- read.table(textConnection(kkk), sep = "\t", header = TRUE,
                 colClasses = c("numeric", "character"))
z2 <- read.table(textConnection(kkk), sep = "\t", header = TRUE,
                 colClasses = c(b = "character", a = "numeric"))
stopifnot(identical(z1, z2))
z3 <- read.table(textConnection(kkk), sep = "\t", header = TRUE,
                 colClasses = c(b = "character"))
stopifnot(identical(z1, z3))
z4 <- read.table(textConnection(kkk), sep = "\t", header = TRUE,
                 colClasses = c(c = "integer", b = "character", a = "numeric"))
stopifnot(identical(z1, z4))
## z2 and z4 used positional matching (and failed) in R < 3.3.0.


## PR#16484
z <- regexpr("(.)", NA_character_, perl = TRUE)
stopifnot(is.na(attr(z, "capture.start")), is.na(attr(z, "capture.length")))
## Result was random integers in R <= 3.2.2.


## PR#14861
if(.Platform$OS.type == "unix") { # no 'ls /'  on Windows
    con <- pipe("ls /", open = "rt")
    data <- readLines(con)
    z <- close(con)
    print(z)
    stopifnot(identical(z, 0L))
}
## was NULL in R <= 3.2.2


## Sam Steingold:  compiler::enableJIT(3) not working in ~/.Rprofile anymore
stopifnot(identical(topenv(baseenv()),
                    baseenv()))
## accidentally globalenv in R 3.2.[12] only


## widths of unknown Unicode characters
stopifnot(nchar("\u200b", "w") == 0)
## was -1 in R 3.2.2


## abbreviate dropped names in some cases
x <- c("AA", "AB", "AA", "CBA") # also test handling of duplicates
for(m in 2:0) {
    print(y <- abbreviate(x, m))
    stopifnot(identical(names(y), x))
}
## dropped for 0 in R <= 3.2.2


## match(<NA>, <NA>)
stopifnot(
    isTRUE(NA          %in% c(NA, TRUE)),
    isTRUE(NA_integer_ %in% c(TRUE, NA)),
    isTRUE(NA_real_    %in% c(NA, FALSE)),# !
    isTRUE(!(NaN       %in% c(NA, FALSE))),
    isTRUE(NA          %in% c(3L, NA)),
    isTRUE(NA_integer_ %in% c(NA, 3L)),
    isTRUE(NA_real_    %in% c(3L, NA)),# !
    isTRUE(!(NaN       %in% c(3L, NA))),
    isTRUE(NA          %in% c(2., NA)),# !
    isTRUE(NA_integer_ %in% c(NA, 2.)),# !
    isTRUE(NA_real_    %in% c(2., NA)),# !
    isTRUE(!(NaN       %in% c(2., NA))))
## the "!" gave FALSE in R-devel (around 20.Sep.2015)


## oversight in  within.data.frame()  [R-help, Sep 20 2015 14:23 -04]
df <- data.frame(.id = 1:3 %% 3 == 2, a = 1:3)
d2 <- within(df, {d = a + 2})
stopifnot(identical(names(d2), c(".id", "a", "d")))
## lost the '.id' column in R <= 3.2.2


## system() truncating and splitting long lines of output, PR#16544
op <- options(warn = 2)# no warnings allowed
if(.Platform$OS.type == "unix") { # only works when platform has getline() in stdio.h
    cn <- paste(1:2222, collapse=" ")
    rs <- system(paste("echo", cn), intern=TRUE)
    stopifnot(identical(rs, cn))
}
options(op)


## tail.matrix()
B <- 100001; op <- options(max.print = B + 99)
mat.l <- list(m0  = matrix(, 0,2),
              m0n = matrix(, 0,2, dimnames = list(NULL, paste0("c",1:2))),
              m2  = matrix(1:2,   2,1),
              m2n = matrix(1:2,   2,3, dimnames = list(NULL, paste0("c",1:3))),
              m9n = matrix(1:9,   9,1, dimnames = list(paste0("r",1:9),"CC")),
              m12 = matrix(1:12, 12,1),
              mBB = matrix(1:B, B, 1))
## tail() used to fail for 0-rows matrices m0*
n.s <- -3:3
hl <- lapply(mat.l, function(M) lapply(n.s, function(n) head(M, n)))
tl <- lapply(mat.l, function(M) lapply(n.s, function(n) tail(M, n)))
## Check dimensions of resulting matrices --------------
## ncol:
Mnc <- do.call(rbind, rep(list(vapply(mat.l, ncol, 1L)), length(n.s)))
stopifnot(identical(Mnc, sapply(hl, function(L) vapply(L, ncol, 1L))),
          identical(Mnc, sapply(tl, function(L) vapply(L, ncol, 1L))))
## nrow:
fNR <- function(L) vapply(L, nrow, 1L)
tR <- sapply(tl, fNR)
stopifnot(identical(tR, sapply(hl, fNR)), # head() & tail  both
          tR[match(0, n.s),] == 0, ## tail(*,0) has always 0 rows
          identical(tR, outer(n.s, fNR(mat.l), function(x,y)
              ifelse(x < 0, pmax(0L, y+x), pmin(y,x)))))
for(j in c("m0", "m0n")) { ## 0-row matrices: tail() and head() look like identity
    co <- capture.output(mat.l[[j]])
    stopifnot(vapply(hl[[j]], function(.) identical(co, capture.output(.)), NA),
              vapply(tl[[j]], function(.) identical(co, capture.output(.)), NA))
}

CO1 <- function(.) capture.output(.)[-1] # drop the printed column names
## checking tail(.) rownames formatting
nP <- n.s > 0
for(nm in c("m9n", "m12", "mBB")) { ## rownames: rather [100000,] than [1e5,]
    tf <- file(); capture.output(mat.l[[nm]], file=tf)
    co <- readLines(tf); close(tf)
    stopifnot(identical(# tail(.) of full output == output of tail(.) :
        lapply(n.s[nP], function(n) tail(co, n)),
        lapply(tl[[nm]][nP], CO1)))
}

identCO <- function(x,y, ...) identical(capture.output(x), capture.output(y), ...)
headI <- function(M, n) M[head(seq_len(nrow(M)), n), , drop=FALSE]
tailI <- function(M, n) M[tail(seq_len(nrow(M)), n), , drop=FALSE]
for(mat in mat.l) {
    ## do not capture.output for  tail(<large>, <small negative>)
    n.set <- if(nrow(mat) < 999) -3:3 else 0:3
    stopifnot(
        vapply(n.set, function(n) identCO (head(mat, n), headI(mat, n)), NA),
        vapply(n.set, function(n) identCO (tail (mat, n, addrownums=FALSE),
                                           tailI(mat, n)), NA),
        vapply(n.set, function(n) all.equal(tail(mat, n), tailI(mat, n),
                                            check.attributes=FALSE), NA))
}
options(op)
## end{tail.matrix check} ------------------

## format.data.frame() & as.data.frame.list() - PR#16580
myL <- list(x=1:20, y=rnorm(20), stringsAsFactors = gl(4,5))
names(myL)[1:2] <- lapply(1:2, function(i)
    paste(sample(letters, 300, replace=TRUE), collapse=""))
nD  <- names(myD  <- as.data.frame(myL))
nD2 <- names(myD2 <- as.data.frame(myL, cut.names = 280))
nD3 <- names(myD3 <- as.data.frame(myL, cut.names = TRUE))
stopifnot(nchar(nD) == c(300,300,16), is.data.frame(myD),  dim(myD)  == c(20,3),
	  nchar(nD2)== c(278,278,16), is.data.frame(myD2), dim(myD2) == c(20,3),
	  nchar(nD3)== c(254,254,16), is.data.frame(myD3), dim(myD3) == c(20,3),
	  identical(nD[3], "stringsAsFactors"),
	  identical(nD[3], nD2[3]), identical(nD[3], nD3[3]))

names(myD)[1:2] <- c("Variable.1", "")# 2nd col.name is "empty"
## A data frame with a column that is an empty data frame:
d20 <- structure(list(type = c("F", "G"), properties = data.frame(i=1:2)[,-1]),
                 class = "data.frame", row.names = c(NA, -2L))
stopifnot(is.data.frame(d20), dim(d20) == c(2,2),
	  identical(colnames(d20), c("type", "properties")),
	  identical(capture.output(d20), c("  type", "1    F", "2    G")))
## format(d20) failed in intermediate R versions
stopifnot(identical(names(myD), names(format(head(myD)))),
	  identical(names(myD), c("Variable.1", "", "stringsAsFactors")),
	  identical(rbind.data.frame(2:1, 1:2), ## was wrong for some days
		    data.frame(c.2L..1L. = c(2L, 1L), X1.2 = 1:2)))
## format.data.frame() did not show "stringsAsFactors" in R <= 3.2.2
## Follow up: the new as.data.frame.list() must be careful with 'AsIs' columns:
desc <- structure( c("a", NA, "z"), .Names = c("A", NA, "Z"))
tools::assertError( data.frame(desc = desc, stringsAsFactors = FALSE) )
## however
dd <- data.frame(desc = structure(desc, class="AsIs"),
                 row.names = c("A","M","Z"), stringsAsFactors = FALSE)
## is "legal" (because "AsIs" can be 'almost anything')
dd ## <- did not format nor print correctly in R-devel early Nov.2015
fdesc <- structure(c("a", "NA", "z"), .Names=names(desc), class="AsIs")
stopifnot(identical(format(dd),
                    data.frame(desc = fdesc, row.names = c("A", "M", "Z"))),
          identical(capture.output(dd),
                    c("  desc", "A    a",
                      "M <NA>", "Z    z")))


## var(x) and hence sd(x)  with factor x, PR#16564
tools::assertError(cov(1:6, f <- gl(2,3)))# was ok already
tools::assertWarning(var(f))
tools::assertWarning( sd(f))
## var() "worked" in R <= 3.2.2  using the underlying integer codes


## loess(*, .. weights) - PR#16587
d.loess <-
    do.call(expand.grid,
            c(formals(loess.control)[1:3],
              list(iterations = c(1, 10),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
d.loess $ iterTrace <- (d.loess$ iterations > 1)
## apply(d.loes, 1L, ...) would coerce everything to atomic, i.e, "character":
loess.c.list <- lapply(1:nrow(d.loess), function(i)
    do.call(loess.control, as.list(d.loess[i,])))
set.seed(123)
for(n in 1:6) { if(n %% 10 == 0) cat(n,"\n")
    wt <- runif(nrow(cars))
    for(ctrl in loess.c.list) {
        cars.wt <- loess(dist ~ speed, data = cars, weights = wt,
                         family = if(ctrl$iterations > 1) "symmetric" else "gaussian",
                         control = ctrl)
        cPr  <- predict(cars.wt)
        cPrN <- predict(cars.wt, newdata=cars)
        stopifnot(all.equal(cPr, cPrN, check.attributes = FALSE, tol=1e-14))
    }
}
## gave (typically slightly) wrong predictions in R <= 3.2.2

