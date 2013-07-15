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
class(x) <- "A"
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
stopifnot(t2  / t1 < 20)
## was around 600--850 for R <= 3.0.1


## ftable(<array with unusual dimnames>)
(m <- matrix(1:12, 3,4, dimnames=list(ROWS=paste0("row",1:3), COLS=NULL)))
ftable(m)
## failed to format (and hence print) because of NULL 'COLS' dimnames

## regression test formerly in kmeans.Rd, but result differs by platform
## Artificial example [was "infinite loop" on x86_64; PR#15364]
rr <- c(rep(-0.4, 5), rep(-0.4- 1.11e-16, 14), -.5)
r. <- signif(rr, 12)
try ( k3 <- kmeans(rr, 3, trace=2) ) ## Warning: Quick-Transfer.. steps exceed
try ( k. <- kmeans(r., 3) ) # after rounding, have only two distinct points
      k. <- kmeans(r., 2)   # fine


## regression test incorrectly in example(NA)
xx <- c(0:4)
is.na(xx) <- c(2, 4)
LL <- list(1:5, c(NA, 5:8), c("A","NA"), c("a", NA_character_))
L2 <- LL[c(1,3)]
dN <- dd <- USJudgeRatings; dN[3,6] <- NA
stopifnot(anyNA(xx), anyNA(LL), !anyNA(L2),
          anyNA(dN), !anyNA(dd), !any(is.na(dd)),
          all(c(3,6) == which(is.na(dN), arr.ind=TRUE)))

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
## used to copy over class in R < 3.1.0.


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
## +x was logical in R <= 3.0.1


proc.time()
