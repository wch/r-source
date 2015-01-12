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


## Bugs reported by Radford Neal
x <- pairlist(list(1,2))
x[[c(1,2)]] <- NULL   # wrongly gave an error, referring to misuse
                      # of the internal SET_VECTOR_ELT procedure
stopifnot(identical(x, pairlist(list(1))))

a <- pairlist(10,20,30,40,50,60)
dim(a) <- c(2,3)
dimnames(a) <- list(c("a","b"),c("x","y","z"))
# print(a)              # doesn't print names, not fixed
a[["a","x"]] <- 0
stopifnot(a[["a","x"]] == 0)
## First gave a spurious error, second caused a seg.fault


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
## as.env..() did not work, and as.list(..) gave non-NULL names in R <= 3.1.1

## all.equal() for environments and refClass()es
RR <- setRefClass("Ex", fields = list(nr = "numeric"))
m1 <- RR$new(); m2 <- RR$new(); m3 <- RR$new(nr = pi); m4 <- RR$new(nr=3.14159)
ee <- emptyenv(); e2 <- new.env()
stopifnot(all.equal(ee,ee), identical(ee,ee), !identical(ee,e2), all.equal(ee,e2),
	  identical(m3,m3), !identical(m1,m2),
	  all.equal(m1,m2), !isTRUE(all.equal(m1,m3)), !isTRUE(all.equal(m1,m4)),
	  all.equal(m3,m4, tol=1e-6), grepl("relative difference", all.equal(m3,m4)),
	  TRUE)
## did not work in R <= 3.1.1
e3 <- new.env()
e3$p <- "p"; e2$p <- "p"; ae.p <- all.equal(e2,e3)
e3$q <- "q";              ae.q <- all.equal(e2,e3)
e2$q <- "Q";              ae.Q <- all.equal(e2,e3)
stopifnot(ae.p, grepl("^Length", ae.q), grepl("string mismatch", ae.Q))
e2$q <- "q"; e2$r <- pi; e3$r <- 3.14159265
stopifnot(all.equal(e2,e3),
	  grepl("relative difference", all.equal(e2,e3, tol=1e-10)))
g <- globalenv() # so it now contains itself
l <- list(e = g)
stopifnot(all.equal(g,g),
	  all.equal(l,l))
## these ran into infinite recursion error.


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


## hist(x, breaks=*) with too large bins, PR#15988
set.seed(5); x <- runif(99)
Hist <- function(x, b) hist(x, breaks=b, plot=FALSE)$counts
for(k in 1:5) {
    b0 <- seq_len(k-1)/k
    H.ok <- Hist(x, c(-10, b0, 10))
    for(In in c(1000, 1e9, Inf))
	stopifnot(identical(Hist(x, c(-In, b0, In)), H.ok),
		  identical(Hist(x, c( 0,  b0, In)), H.ok))
}
## "wrong" results for k in {2,3,4} in R <= 3.1.1


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


## missing() did not propagate through '...', PR#15707
check <- function(x,y,z) c(missing(x), missing(y), missing(z))
check1 <- function(...) check(...)
check2 <- function(...) check1(...)
stopifnot(identical(check2(one, , three), c(FALSE, TRUE, FALSE)))
## missing() was unable to handle recursive promises


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


## eigen(*, symmetric = <default>) with asymmetric dimnames,  PR#16151
m <- matrix(c(83,41), 5, 4,
	    dimnames=list(paste0("R",1:5), paste0("C",1:4)))[-5,] + 3*diag(4)
stopifnot( all.equal(eigen(m, only.values=TRUE) $ values,
		     c(251, 87, 3, 3), tol=1e-14) )
## failed, using symmetric=FALSE and complex because of the asymmetric dimnames()


proc.time()
