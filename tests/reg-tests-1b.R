postscript("reg-tests-1b.ps", encoding = "ISOLatin1.enc")

## force standard handling for data frames
options(stringsAsFactors=TRUE)
## .Machine
(Meps <- .Machine$double.eps)# and use it in this file

assertError <- function(expr)
    stopifnot(inherits(try(expr, silent = TRUE), "try-error"))


## str() for list-alikes :
"[[.foo" <- function(x,i) x
x <- structure(list(2), class="foo")
str(x)
## gave infinite recursion < 2.6.0

curve(sin, -2*pi, 3*pi); pu1 <- par("usr")[1:2]
curve(cos); stopifnot(all.equal(par("usr")[1:2], pu1))
## failed in R <= 2.6.0

## tests of side-effects with CHARSXP caching
x <- y <- "abc"
Encoding(x) <- "UTF-8"
stopifnot(Encoding(y) == "unknown") # was UTF-8 in 2.6.0
x <- unserialize(serialize(x, NULL))
stopifnot(Encoding(y) == "unknown") # was UTF-8 in 2.6.0
##  problems in earlier versions of cache


## regression test for adding functions to deriv()
deriv3(~  gamma(y), namevec="y")
deriv3(~  lgamma(y), namevec="y")
# failed in R < 2.7.0
D(quote(digamma(sin(x))),"x")
D(quote(trigamma(sin(x))),"x")
D(quote(psigamma(sin(x))),"x")
D(quote(psigamma(sin(x), 3)),"x")
n <- 2L; D(quote(psigamma(sin(x), n)),"x")
## rest are new


## .subset2 quirk
iris[1, c(TRUE, FALSE, FALSE, FALSE, FALSE)]
iris[1, c(FALSE, FALSE, FALSE, FALSE, TRUE)]
## failed in 2.6.0


## indexing by "": documented as 'no name' and no match
x <- structure(1:4, names=c(letters[1:3], ""))
stopifnot(is.na(x[""])) # always so
stopifnot(is.na(x[NA_character_]))
z <- tryCatch(x[[NA_character_]], error=function(...) {})
stopifnot(is.null(z))
z <- tryCatch(x[[""]], error=function(...) {})
stopifnot(is.null(z)) # x[[""]] == 4 < 2.7.0
x[[""]] <- 5  # no match, so should add an element, but replaced.
stopifnot(length(x) == 5)
x[""] <- 6    # also add
stopifnot(length(x) == 6)
xx <- list(a=1, 2)
stopifnot(is.null(xx[[""]])) # 2 < 2.7.0
##


## negative n gave choose(n, k) == 0
stopifnot(isTRUE(all.equal(choose(-1,3),-1)))
##


## by() on 1-column data frame (PR#10506)
X <- data.frame(a=1:10)
g <- gl(2,5)
by(X, g, colMeans)
## failed in 2.6.1


## range.default omitted na.rm on non-numeric objects
(z <- range(as.Date(c("2007-11-06", NA)), na.rm = TRUE))
stopifnot(!is.na(z))
## NAs in 2.6.1


## cut() on constant values used the min, not abs(min)
z <- cut(rep(-1,5), 2)
stopifnot(!is.na(z))
##


## extreme example of two-sample wilcox.test
## reported by Wolfgang Huber to R-devel, 2008-01-01
## normal approximation is way off here.
wilcox.test(1, 2:60, conf.int=TRUE, exact=FALSE)
## failed in R < 2.7.0


## more corner cases for cor()
z <- cor(c(1,2,3),c(3,4,6),use="pairwise.complete.obs",method="kendall")
stopifnot(!is.matrix(x)) # was 1x1 in R < 2.7.0
Z <- cbind(c(1,2,3),c(3,4,6))
# next gave 0x0 matrix < 2.7.0
z <- try(cor(Z[, FALSE], use="pairwise.complete.obs",method="kendall"))
stopifnot(inherits(z, "try-error"))
# next gave NA < 2.7.0
z <- try(cor(numeric(0), numeric(0), use="pairwise.complete.obs",
             method="kendall"))
stopifnot(inherits(z, "try-error"))
##


## infinite loop in format.AsIs reported on R-help by Bert Gunter
## https://stat.ethz.ch/pipermail/r-help/2008-January/149504.html
z <- rep(Sys.time(),5)
data.frame(I(z))
##


## drop with length-one result
x <- matrix(1:4, 4,1, dimnames=list(letters[1:4], NULL))
stopifnot(identical(names(drop(x)), letters[1:4])) # was OK
stopifnot(identical(names(drop(x[1,,drop=FALSE])), "a")) # was no names
stopifnot(identical(names(x[1,]), "a")) # ditto
# now consistency tests.
x <- matrix(1, 1, 1, dimnames=list("a", NULL))
stopifnot(identical(names(x[,]), "a"))
x <- matrix(1, 1, 1, dimnames=list(NULL, "a"))
stopifnot(identical(names(x[,]), "a"))
x <- matrix(1, 1, 1, dimnames=list("a", "b"))
stopifnot(is.null(names(x[,])))
## names were dropped in R < 2.7.0 in all cases except the first.


## fisher.test with extreme degeneracy PR#10558
a <- diag(1:3)
p <- fisher.test(a, simulate.p.value=TRUE)$p.value
# true value is 1/60, but should not be small
stopifnot(p > 0.001)
## was about 0.0005 in 2.6.1 patched


## tests of problems fixed by Marc Schwartz's patch for
## cut/hist for Dates and POSIXt
Dates <- seq(as.Date("2005/01/01"), as.Date("2009/01/01"), "day")
months <- format(Dates, format = "%m")
years <- format(Dates, format = "%Y")
mn <- as.vector(unlist(sapply(split(months, years), table)))
ty <- as.vector(table(years))
# Test hist.Date() for months
stopifnot(identical(hist(Dates, "month", plot = FALSE)$counts, mn))
# Test cut.Date() for months
stopifnot(identical(as.vector(table(cut(Dates, "month"))), mn))
# Test cut.Date() for 3 months
stopifnot(identical(as.vector(table(cut(Dates, "3 months"))),
                    as.integer(colSums(matrix(c(mn, 0, 0), nrow = 3)))))
# Test hist.Date() for years
stopifnot(identical(hist(Dates, "year", plot = FALSE)$counts, ty))
# Test cut.Date() for years
stopifnot(identical(as.vector(table(cut(Dates, "years"))),ty))
# Test cut.Date() for 3 years
stopifnot(identical(as.vector(table(cut(Dates, "3 years"))),
                    as.integer(colSums(matrix(c(ty, 0), nrow = 3)))))

Dtimes <- as.POSIXlt(Dates)
# Test hist.POSIXt() for months
stopifnot(identical(hist(Dtimes, "month", plot = FALSE)$counts, mn))
# Test cut.POSIXt() for months
stopifnot(identical(as.vector(table(cut(Dtimes, "month"))), mn))
# Test cut.POSIXt() for 3 months
stopifnot(identical(as.vector(table(cut(Dtimes, "3 months"))),
                    as.integer(colSums(matrix(c(mn, 0, 0), nrow = 3)))))
# Test hist.POSIXt() for years
stopifnot(identical(hist(Dtimes, "year", plot = FALSE)$counts, ty))
# Test cut.POSIXt() for years
stopifnot(identical(as.vector(table(cut(Dtimes, "years"))), ty))
# Test cut.POSIXt() for 3 years
stopifnot(identical(as.vector(table(cut(Dtimes, "3 years"))),
                    as.integer(colSums(matrix(c(ty, 0), nrow = 3)))))
## changed in 2.6.2


## zero-length args in tapply (PR#10644)
tapply(character(0), factor(letters)[FALSE], length)
## failed < 2.6.2


## zero-length patterns in gregexpr
expect <- structure(1:3, match.length=rep(0L, 3))
stopifnot(identical(expect, gregexpr("", "abc")[[1]]))
stopifnot(identical(expect, gregexpr("", "abc", fixed=TRUE)[[1]]))
stopifnot(identical(expect, gregexpr("", "abc", perl=TRUE)[[1]]))
## segfaulted < 2.6.2


## test of internal argument matching
stopifnot(all.equal(round(d=2, x=pi), 3.14))
## used positional matching in 2.6.x


## kappa.tri(x, exact=TRUE) wrongly ended using exact=FALSE:
data(longley)
fm1 <- lm(Employed ~ ., data = longley)
stopifnot(all.equal(23845862, kappa(fm1, exact=TRUE)))


## names from pairlists (PR#10807, esoteric)
m <- c("a", "b", "c")
mp <- pairlist("a", "b", "c")
x <- 1:3
names(x) <- mp
stopifnot(identical(names(x), m)) # OK before
x <- 1:3
attr(x, "names") <- mp
stopifnot(identical(names(x), m)) # rep("a", 3) in 2.6.x
##


## preserving attributes in [<-.data.frame (PR#10873)
df <- data.frame(a=1:3, b=letters[1:3])
attr(df,"foo") <- 10
df[, "b"] <- 10:12
stopifnot(identical(attr(df, "foo"), 10))
## dropped attributes < 2.7.0


## r<foo> NA warnings, and rnorm(*, mu = +- Inf) consistency
op <- options(warn=2)
m <- c(-Inf,Inf)
stopifnot(rnorm(2, mean = m) == m,
          rexp (2, Inf) == 0)
rt(1, Inf)
R <- list(try(rnorm(2, numeric())),
          try(rexp (2, numeric())),
          try(rnorm(2, c(1,NA))),
          try(rnorm(1, sd = Inf)) )
options(op)
stopifnot(sapply(R, function(ch) sub(".* : ", '', ch) ==
                 "(converted from warning) NAs produced\n"))
## was inconsistent in R < 2.7.0


## predict.loess with transformed variables
y <- 1:100 + rnorm(100)
od <- data.frame(x=1:100, z=1:100 + rnorm(100, 10))
nd <- data.frame(x=1:100, z=11:110)
fit <- loess(y ~ log(x) + log(z), od)
p1 <- predict(fit, nd) # failed in 2.6.x
fit.log <- loess(y ~ x + z, log(od))
p2 <- predict(fit.log, log(nd))
stopifnot(identical(p1,p2))


## wishlist PR#11192
plot(1:10)
segments(1, 1, 10, 10, col='green')
segments(numeric(0), numeric(0), numeric(0), numeric(0), col='green')
## last was error in R < 2.8.0


## merging with a zero-row data frame
merge(NULL, women)
merge(women, NULL)
merge(women[FALSE, ], women)
merge(women, women[FALSE, ])
## first two failed in 2.7.0


## influence.measures() for lm and glm, and its constituents
if(require(MASS)) {
    fit <- lm(formula = 1000/MPG.city ~ Weight + Cylinders + Type + EngineSize + DriveTrain, data = Cars93)
    gf <- glm(formula(fit), data=Cars93) # should be "identical"
    im1 <- influence.measures(fit)
    im2 <- influence.measures(gf)
    stopifnot(all.equal(im1[1:2], im2[1:2]),
	      all.equal(unname(im1$infmat[,1:15]), unname(dfbetas(fit))),
	      all.equal(im1$infmat[,"dffit"], dffits(fit)),
	      all.equal(im1$infmat[,"cov.r"], covratio(fit)),
	      all.equal(im1$infmat[,"cook.d"], cooks.distance(fit)),
	      all.equal(im2$infmat[,"cook.d"], cooks.distance(gf)),
	      all.equal(im1$infmat[,"hat"],  hatvalues(fit)))
}
## "cook.d" part of influence.measures(<glm>) differed in R <= 2.7.0


## short list value for dimnames
n <- matrix(c(1259, 845, 719,390,1360,1053,774,413), nrow = 2, byrow = TRUE)
dimnames(n)[[1]] <- c("a", "b")
## was (correctly) an error in R < 2.8.0


## glob2rx(pattern, .) with "(", "[" or "{" in pattern :
nm <- "my(ugly[file{name"
stopifnot(identical(regexpr(glob2rx("*[*"), nm),
		    structure(1L, match.length = 8L)),
	  identical(regexpr(glob2rx("*{n*"), nm),
		    structure(1L, match.length = 14L)),
	  identical(regexpr(glob2rx("*y(*{*"), nm),
		    structure(1L, match.length = 13L))
	  )
## gave 'Invalid regular expression' in R <= 2.7.0


## showDefault() problem with "unregistered" S3 classes:
show(structure(1:3, class = "myClass"))
## failed in R <= 2.7.0


## formatC(.., format="fg", flag="#"):
x <- 0.599 * c(.1, .01, .001, 1e-4,1e-5,1e-6)
(fCx <- formatC(x, digits=2, format="fg", flag="#"))
stopifnot(sub(".*(..)$", "\\1", fCx) == "60")
## dropped the trailing "0" in the last 3 cases, in R <= 2.7.0


## c.noquote bug, posted to R-devel by Ray Brownrigg, 2008-06-16
z <- c(noquote('z'), 'y', 'x', 'w')
stopifnot(identical(unclass(z), c('z', 'y', 'x', 'w')))
## repeated third and later args in R < 2.7.1.

## PD found that f==f contains NA when f has NA levels (but no missing value)
f1 <- factor(c(1, 2, NA), exclude = "")
f2 <- factor(c(1, 2, NA), exclude = NULL)
stopifnot(identical(f1, factor(c(1,2,NA))),
          nlevels(f1) == 2, nlevels(f2) == 3,
          all(f2 == f2), !any(f2 != f2),
          identical(f1 == f1, c(TRUE,TRUE,NA)))

f. <- f <- factor(c(letters[c(1:3,3:1)],"NA", "d","d", NA), exclude=NULL)
is.na(f.)[2:3] <- TRUE
f.
stopifnot(all(f == f), identical(f == f., f. == f.),
          identical(2:3, which(is.na(f. == f.))))
## f == f was wrong in R 1.5.0 -- 2.7.1


## data.frame[, <char>] must match exactly
dd <- data.frame(ii = 1:10, xx = pi * -3:6)
t1 <- try(dd[,"x"])# partial match
t2 <- try(dd[,"C"])# no match
stopifnot(inherits(t1, "try-error"),
	  inherits(t2, "try-error"),
	  ## partial matching is "ok" for '$' {hence don't use for dataframes!}
	  identical(dd$x, dd[,"xx"]))
## From 2.5.0 to 2.7.1, the non-match indexing gave NULL instead of error

## data.frame[ (<NA>), ] when row.names had  "NA"
x <- data.frame(x=1:3, y=2:4, row.names=c("a","b","NA"))
y  <- x [c(2:3, NA),]
y.ok <- data.frame(x=c(2:3,NA), y=c(3:4,NA), row.names=c("b", "NA", "NA.1"))
stopifnot(identical(y, y.ok))
## From 2.5.0 to 2.7.1,  y had row name "NA" twice

stopifnot(shapiro.test(c(0,0,1))$p.value >= 0)
## was wrong up to 2.7.1, because of rounding errors (in single precision).

stopifnot(rcond(cbind(1, c(3,3))) == 0)
## gave an error (because Lapack's LU detects exact singularity)


## dispatch when primitives are called from lapply.
x <- data.frame(d=Sys.Date())
stopifnot(sapply(x, is.numeric) == FALSE)
# TRUE in 2.7.1, tried to dispatch on "FUN"
(ds <- seq(from=Sys.Date(), by=1, length=4))
lapply(list(d=ds), round)
# failed in 2.7.1 with 'dispatch error' since call had '...' arg
## related to calls being passed unevaluated by lapply.


## subsetting data frames with NA cols
## Dieter Menne: https://stat.ethz.ch/pipermail/r-help/2008-January/151266.html
df3 <- data.frame(a=0:10,b=10:20,c=20:30)
names(df3) <- c("A","B", NA)
df3[-2]
df3[, -2]
df3[1:4, -2]
df3[c(TRUE,FALSE,TRUE)]
df3[, c(TRUE,FALSE,TRUE)]
df3[1:4, c(TRUE,FALSE,TRUE)]
## all gave 'undefined columns selected', 2.6.1 to 2.7.x
## note that you can only select columns by number, not by name


## nls with weights in an unusual model
Data <- data.frame(x=c(1,1,1,1,1,2,2,3,3,3,3,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,
                   7,7,7,7,7,7,7,7,7,8,8,8, 8,8,8,8,8,8,8,9,9,9,9,9,11,12),
                   y=c(73,73,70,74,75,115,105,107,124,107,116,125,102,144,178,
                   149,177,124,157,128, 169,165,186,152,181,139,173,151,138,
                   181,152,188,173,196,180,171,188,174,198, 172, 176,162,188,
                   182,182,141,191,190,159,170,163,197),
                   weight=c(1, rep(0.1, 51)))
G.st <- c(k=0.005, g1=50,g2=550)
# model has length-1 (and 52) variables
Ta <- min(Data$x)
Tb <- max(Data$x)

#no weights
nls(y~((g1)*exp((log(g2/g1))*(1-exp(-k*(x-Ta)))
                /(1-exp(-k*(Tb-Ta))))), data=Data, start=G.st, trace=TRUE)

#with weights
nls(y~((g1)*exp((log(g2/g1))*(1-exp(-k*(x-Ta)))
                /(1-exp(-k*(Tb-Ta))))), data=Data, start=G.st,
    trace=TRUE, weights=weight)
## failed for find weights in R <= 2.7.1


## barplot(log = "y") with NAs (PR#11585)
dat <- matrix(1:25, 5)
dat[2,3] <- NA
barplot(dat, beside = TRUE, log = "y")
## failed in 2.7.1


## related to PR#12551
unique("a", c("a", "b"))
unique(1, 1:2)
# could seqfault in 2.7.1 on some platforms
stopifnot(!duplicated(rep("a", 3), "a"))
## wrong answer in 2.7.1


## drop1.lm() bug
dd <- stackloss ; dd[1,3] <- NA
rr <- lm(stack.loss ~ ., data=dd, na.action=na.exclude)
drop1(rr)
## failed in 2.7.x


## explicit row.names=NULL in data.frame()
stopifnot(identical(row.names(data.frame(x=c(a=1,b=2), row.names=NULL)),
                    c("1", "2")))
stopifnot(identical(row.names(data.frame(x=c(a=1,b=2))), c("a", "b")))
## same as default in 2.5.0 <= R < 2.7.2

stopifnot(all.equal(chol2inv(2), matrix(0.25, 1), tol = 4*Meps),
	  all.equal(solve(chol2inv(chol(4))), matrix(4, 1), tol = 10*Meps))
## chol2inv() did not accept non-matrices up to 2.7.*


## seek should discard pushback. (PR#12640)
cat(c("1\t2\t3", "4\t5\t6"), file="foo.txt", sep="\n")
fd <- file("foo.txt",open="rt")
scan(file=fd,what=double(),n=2)
seek(con=fd,where=0,origin="start")
z <- scan(file=fd,what=double(),n=2)
close(fd)
unlink("foo.txt")
stopifnot(identical(z, c(1,2)))
## changed in 2.7.2 patched

## cov / cor / var etc with NAs :
stopifnot(inherits(try(var(NULL)), "try-error"))## gave NA in 1.2.2
v0 <- var(0[FALSE]) # gave "'x' is empty" in the past;  NA in 1.2.2
x <- c(1:2,NA)
v1 <- var(c(1,NA))
v2 <- var(c(NA,0/0, Inf-Inf))
sx <- sd(x)# sd() -> var()
## all three gave "missing observations in cov/cor"  for a long time in the past
is.NA <- function(x) is.na(x) & !is.nan(x)
stopifnot(is.NA(v1), is.NA(v2), is.NA(sx),
	  all.equal(0.5, var(x, na.rm=TRUE), tol=8*Meps)# should even be exact
	  )

## write.dcf() indenting for ".<foo>" (PR#12816)
zz <- textConnection("foo", "w")
write.dcf(list(Description = 'what a fat goat .haha'),
          file = zz, indent=1, width=10)
stopifnot(substring(foo[-1], 1,1) == " ", length(foo) == 4,
          foo[4] == "  .haha")
close(zz)
## was " .haha" (not according to DCF standard)


## Pdf() with CIDfonts active -- they need MBCS to be supported
pdf(family="Japan1") # << for CIDfonts, pd->fonts is NULL
try({
    plot(1,1,pch="", axes=FALSE)
    text(1,1,"F.1", family="Helvetica")
})
dev.off()
## text() seg.faulted up to 2.7.2 (and early 2.8.0-alpha)

## PS mixing CIDfonts and Type1 - reverse case
postscript(family="Helvetica")
plot(1,1,pch="", axes=FALSE)
try(text(1,1,"A",family="Japan1"))
## error instead of seg.fault


## splinefun with derivatives evaluated to the left of first knot
x <- 1:10; y <- sin(x)
splfun <- splinefun(x,y, method='natural')
x1 <- splfun( seq(0,1, 0.1), deriv=1 )
x2 <- splfun( seq(0,1, 0.1), deriv=2 )
x3 <- splfun( seq(0,1, 0.1), deriv=3 )
stopifnot(x1 == x1[1], x2 == 0, x3 == 0)
##


## glm(y = FALSE), in part PR#1398
fit <- glm(1:10 ~ I(1:10) + I((1:10)^2), y = FALSE)
anova(fit)
## obscure errors < 2.8.0


## boundary case in cut.Date (PR#13159)
d <- as.Date("2008-07-07")
cut(d, "weeks")
d <- as.POSIXct("2008-07-07", tz="UTC")
cut(d, "weeks")
## failed < 2.8.0


### end of tests added for 2.8.x


## (Deliberate) overshot in seq(from, to, by) because of fuzz
stopifnot(seq(0, 1, 0.00025+5e-16) <= 1, seq.int(0, 1, 0.00025+5e-16) <= 1)
## overshot by about 2e-12 in 2.8.x
## no longer reaches 1 in 2.11.0 (needed a fuzz of 8e-9)


## str() with an "invalid object"
ob <- structure(1, class = "test") # this is fine
is.object(ob)# TRUE
ob <- 1 + ob # << this is "broken"
is.object(ob)# FALSE - hmm..
identical(ob, unclass(ob)) # TRUE !
stopifnot(grep("num 2", capture.output(str(ob))) == 1)
## str(ob) lead to infinite recursion in R <= 2.8.0


## row.names(data.frame(matrixWithDimnames)) (PR#13230)
rn0 <- c("","Row 2","Row 3")
A <- matrix(1:6, nrow=3, ncol=2, dimnames=list(rn0, paste("Col",1:2)))
rn <- row.names(data.frame(A))
stopifnot(identical(rn, rn0))
# was 1:3 in R 2.8.0, whereas
rn0 <- c("Row 1","","Row 3")
A <- matrix(1:6, nrow=3, ncol=2, dimnames=list(rn0, paste("Col",1:2)))
rn <- row.names(data.frame(A))
stopifnot(identical(rn, rn0))
## used the names.


## rounding error in windowing a time series (PR#13272)
x <- ts(1:290, start=c(1984,10), freq=12)
window(x, start=c(2008,9), end=c(2008,9), extend=FALSE)
window(x, start=c(2008,9), end=c(2008,9), extend=TRUE)
## second failed in 2.8.0


## deparse(nlines=) should shrink the result (PR#13299)
stopifnot(length(deparse(quote(foo(1,2,3)), width.cutoff = 20, nlines=7)) ==1)
## was 7.


## legend did not reset xpd correctly (PR#12756)
par(xpd = FALSE)
plot(1)
legend("top", legend="Tops", xpd=NA, inset=-0.1)
stopifnot(identical(par("xpd"), FALSE))
## left xpd as NA


## lines.formula with 'subset' and no 'data' needed a tweak
## (R-help, John Field, 20008-11-14)
x <- 1:5
y <- c(1,3,NA,2,5)
plot(y ~ x, type="n")
lines(y ~ x, subset = !is.na(y), col="red")
## error in 2.8.0


## prettyNum(*, drop0trailing) erronously dropped 0 in '1e10':
cn <- c("1.107", "2.3120", "3.14e+0", "4.2305400", "120.0",
        "5.31e-01", "6.3333e-20", "8.1e100", "9.9e+00", "10.1e-0")
d <- cn != (pcn <- prettyNum(cn, drop0trailing=TRUE))
stopifnot(identical(pcn[d],
		    c("2.312", "3.14", "4.23054","120","9.9","10.1")),
	  identical("-3", prettyNum("-3.0",drop0trailing=TRUE)) )
## first failed, e.g. for 8.1e100


## (R-help, 2008-12-01)
transform(mtcars, t1=3, t2=4)
## failed in 2.8.0 since extra columns were passed as a list.


## deparsing transform failed
parse(text = deparse(transform))
## failed in 2.8.0


## crashed on some systems (PR#13361)
matrix(1:4, nrow=2, dimnames=list())
##


## col(as.factor=TRUE) failed
col(matrix(0, 5, 5), as.factor=TRUE)
## failed in 2.8.0


## qt failure in R-devel in early Dec 2008
stopifnot(!is.nan(qt(0.1, 0.1)))
##


## formals<- gave wrong result for list body
f <- f0 <- function(x) list(pi)
formals(f) <- formals(f)
stopifnot(identical(body(f), body(f)))
## had body 'pi' < 2.8.1


## body<- failed on a function with no arguments.
f <- function() {pi}
body(f) <- 2
f
## Failed < 2.8.1


## body<- with value a list
f <- function(x) NULL
body(f) <- list(pi)
stopifnot(is.list(body(f))) # was 'pi'
body(f) <- b0 <- list(a=1, b=2)
stopifnot(identical(body(f), b0)) # 'a' became an argument
f <- function(x) NULL
body(f) <- list(1, 2, 3) # was error
## pre-2.9.0 behaviour was erratic.


## PR#13305
qr.solve(cbind(as.complex(1:11), as.complex(1)),
         as.complex(2*(20:30)))
## failed in 2.8.1


## PR#13433: is ....\nEOF an empty last line?
aa <- "field1\tfield2\n 1\ta\n 2\tb"
zz <- textConnection(aa)
res <- read.table(zz, blank.lines.skip = FALSE)
close(zz)
stopifnot(nrow(res) == 3)
## was 4 in 2.8.1


## segfault from cbind() reported by Hadley Wickham
## https://stat.ethz.ch/pipermail/r-devel/2009-January/051853.html
e <- environment()
a <- matrix(list(e), ncol = 1, nrow = 2)
b <- matrix(ncol = 0, nrow = 2) # zero-length
cbind(a, b)
cbind(a, b)
## crashed in 2.9.0


## besselI(x, -n) == besselI(x, +n)  when n is an integer
set.seed(7) ; x <- rlnorm(216) ; nu <- c(1,44,111)
## precision lost warnings {may be gone in the future}:
suppressWarnings(r <- outer(x, c(-nu, nu), besselI))
stopifnot(identical(r[,1:3], r[,4:6]))
## suffered from sin(n * pi) imprecision in R <= 2.8.1


## Large sanples in mood.test
## https://stat.ethz.ch/pipermail/r-help/2009-March/190479.html
set.seed(123)
x <- rnorm(50, 10, 5)
y <- rnorm(50, 2 ,5)
(z <- mood.test(x, y))
stopifnot(!is.na(z$p.value))
## gave warning and incorrect result in 2.8.x


## heatmap without dendrogram (PR#13512)
X <- matrix(rnorm(200),20,10)
XX <- crossprod(X)
heatmap(XX, Rowv =  NA, revC = TRUE)
heatmap(XX, Rowv = NA, symm = TRUE)
## both failed in 2.8.1


## sprintf with 0-length args
stopifnot(identical(sprintf("%d", integer(0L)), character(0L)))
stopifnot(identical(sprintf(character(0L), pi), character(0L)))
## new feature in 2.9.0


## C-level asLogical(x) or c(<raw>, <number>) did not work
r <- as.raw(1)
stopifnot(if(r) TRUE)
for (type in c("null", "logical", "integer", "real", "complex",
               "character", "list", "expression"))
    c(r, r, get(sprintf('as.%s', type))(1))
## failed  before 2.9.0


### Non-unique levels in factor should be forbidden from R 2.10.0 on
c1 <- c("a.b","a"); c2 <- c("c","b.c")
fi <- interaction(c1, c2)
stopifnot(length(lf <- levels(fi)) == 3, lf[1] == "a.b.c",
	  identical(as.integer(fi), rep.int(1L, 2)))
## interaction() failed to produce unique levels before 2.9.1

levs <- c("A","A")
## warnings for now {errors in the future}
local({ oo <- options(warn=2); on.exit(options(oo))
	assertError(gl(2,3, labels = levs))
	assertError(factor(levs, levels=levs))
	assertError(factor(1:2,	 labels=levs))
    })
## failed in R < 2.10.0
L <- c("no", "yes")
x <- (5:1)/10; lx <- paste("0.", 1:5, sep="")
y <- pi + (-9:9)*2^-53
z <- c(1:2,2:1) ; names(z) <- nz <- letters[seq_along(z)]
of <- ordered(4:1)
stopifnot(identical(factor(c(2, 1:2), labels = L),
		    structure(c(2L, 1:2), .Label = L, class="factor")),
	  identical(factor(x),
		    structure(5:1, .Label = lx, class="factor")),
	  length(levels(factor(y))) == 1, length(unique(y)) == 5,
	  identical(factor(z),
		    structure(z, .Names = nz, .Label = c("1","2"),
			      class="factor")),
	  identical(of, factor(of)))
## partly failed in R <= 2.9.0, partly in R-devel(2.10.0)


## "misuses" of sprintf()
assertError(sprintf("%S%"))
assertError(sprintf("%n %g", 1))
## seg.faulted in R <= 2.9.0


## sprintf(., e)  where length(as.character(e)) < length(e):
e <- tryCatch(stop(), error=identity)
stopifnot(identical(sprintf("%s", e),
		    sprintf("%s", as.character(e))))
## seg.faulted in R <= 2.9.0
e <- tryCatch(sprintf("%q %d",1), error=function(e)e)
e2 <- tryCatch(sprintf("%s", quote(list())), error=function(e)e)
e3 <- tryCatch(sprintf("%s", quote(blabla)), error=function(e)e)
stopifnot(inherits(e, "error"), inherits(e2, "error"),inherits(e3, "error"),
	  grep("invalid", c(msg	 <- conditionMessage(e),
			    msg2 <- conditionMessage(e2),
			    msg3 <- conditionMessage(e3))) == 1:3,
	  1 == c(grep("%q", msg), grep("language", msg2), grep("symbol", msg3))
          )
## less helpful error messages previously


## bw.SJ on extreme example
ep <- 1e-3
stopifnot(all.equal(bw.SJ(c(1:99, 1e6), tol=ep), 0.725, tol=ep))
## bw.SJ(x) failed for R <= 2.9.0 (in two ways!), when x had extreme outlier


## anyDuplicated() with 'incomp' ...
oo <- options(warn=2) # no warnings allowed
stopifnot(identical(0L, anyDuplicated(c(1,NA,3,NA,5), incomp=NA)),
	  identical(5L, anyDuplicated(c(1,NA,3,NA,3), incomp=NA)),
	  identical(4L, anyDuplicated(c(1,NA,3,NA,3), incomp= 3)),
	  identical(0L, anyDuplicated(c(1,NA,3,NA,3), incomp=c(3,NA))))
options(oo)
## missing UNPROTECT and partly wrong in development versions of R


## test of 'stringsAsFactors' argument to expand.grid()
z <- expand.grid(letters[1:3], letters[1:4], stringsAsFactors = TRUE)
stopifnot(sapply(z, class) == "factor")
z <- expand.grid(letters[1:3], letters[1:4], stringsAsFactors = FALSE)
stopifnot(sapply(z, class) == "character")
## did not work in 2.9.0, fixed in 2.9.1 patched


## print.srcref should not fail; a bad encoding should fail; neither should
## leave an open connection
nopen <- nrow(showConnections())
tmp <- tempfile()
cat( c( "1", "a+b", "2"), file=tmp, sep="\n")
p <- parse(tmp)
print(p)
con <- try(file(tmp, open="r", encoding="unknown"))
unlink(tmp)
stopifnot(inherits(con, "try-error") && nopen == nrow(showConnections()))
##


## PR#13574
x <- 1:11; y <- c(6:1, 7, 11:8)
stopifnot(all.equal(cor.test(x, y, method="spearman", alternative="greater")$p.value, cor.test(x, -y, method="spearman", alternative="less")$p.value))
## marginally different < 2.9.0 patched


## median should work on POSIXt objects (it did in 2.8.0)
median(rep(Sys.time(), 2))
## failed in 2.8.1, 2.9.0


## repeated NA in dim() (PR#13729)
L0 <- logical(0)
try(dim(L0) <- c(1,NA,NA))
stopifnot(is.null(dim(L0)))
L1 <- logical(1)
try(dim(L1) <- c(-1,-1))
stopifnot(is.null(dim(L)))
## dim was set in 2.9.0


## as.character(<numeric>)
nx <- 0.3 + 2e-16 * -2:2
stopifnot(identical("0.3", unique(as.character(nx))),
          identical("0.3+0.3i", unique(as.character(nx*(1+1i)))))
## the first gave ("0.300000000000000" "0.3") in R < 2.10.0


## aov evaluated a test in the wrong place ((PR#13733)
DF <- data.frame(y = c(rnorm(10), rnorm(10, mean=3), rnorm(10, mean=6)),
                 x = factor(rep(c("A", "B", "C"), c(10, 10, 10))),
                 sub = factor(rep(1:10, 3)))
## In 2.9.0, the following line raised an error because "x" cannot be found
junk <- summary(aov(y ~ x + Error(sub/x), data=DF, subset=(x!="C")))
## safety check added in 2.9.0 evaluated the call.


## for(var in seq) .. when seq is modified  "inside" :
x <- c(1,2); s <- 0; for (i in x) { x[i+1] <- i + 42.5; s <- s + i }
stopifnot(s == 3)
## s was  44.5  in R <= 2.9.0


## ":" at the boundary
M <- .Machine$integer.max
s <- (M-2):(M+.1)
stopifnot(is.integer(s), s-M == -2:0)
## was "double" in R <= 2.9.1


## too many columns model.matrix()
dd <- as.data.frame(sapply(1:40, function(i) gl(2, 100)))
(f <- as.formula(paste("~ - 1 + ", paste(names(dd), collapse = ":"), sep = "")))
e <- tryCatch(X <- model.matrix(f, data = dd), error=function(e)e)
stopifnot(inherits(e, "error"))
## seg.faulted in R <= 2.9.1


## seq_along( <obj> )
x <- structure(list(a = 1, value = 1:7), class = "FOO")
length.FOO <- function(x) length(x$value)
stopifnot(identical(seq_len(length(x)),
		    seq_along(x)))
## used C-internal non-dispatching length() in R <= 2.9.1


## factor(NULL)
stopifnot(identical(factor(), factor(NULL)))
## gave an error from R ~1.3.0 to 2.9.1


## methods() gave two wrong warnings in some cases:
op <- options(warn = 2)# no warning, please!
m1 <- methods(na.omit) ## should give (no warning):
##
setClass("bla")
setMethod("na.omit", "bla", function(object, ...) "na.omit(<bla>)")
(m2 <- methods(na.omit)) ## should give (no warning):
stopifnot(identical(m1, m2))
options(op)
## gave two warnings, when an S3 generic had turned into an S4 one


## raw vector assignment with NA index
x <- charToRaw("abc")
y <- charToRaw("bbb")
x[c(1, NA, 3)] <- x[2]
stopifnot(identical(x, y))
## used to segfault


## Logic operations with complex
stopifnot(TRUE & -3i, FALSE | 0+1i,
	  TRUE && 1i, 0+0i || 1+0i)
## was error-caught explicitly in spite of contrary documentation


## Tests of save/load with different types of compression
x <- xx <- 1:1000
test1 <- function(ascii, compress)
{
    tf <- tempfile()
    save(x, ascii = ascii, compress = compress, file = tf)
    load(tf)
    stopifnot(identical(x, xx))
    unlink(tf)
}
for(compress in c(FALSE, TRUE))
    for(ascii in c(TRUE, FALSE)) test1(ascii, compress)
for(compress in c("bzip2", "xz"))
    for(ascii in c(TRUE, FALSE)) test1(ascii, compress)


## tests of read.table with different types of compressed input
mor <- system.file("data/morley.tab", package="datasets")
ll <- readLines(mor)
tf <- tempfile()
## gzip copression
writeLines(ll, con <- gzfile(tf)); close(con)
file.info(tf)$size
stopifnot(identical(read.table(tf), morley))
## bzip2 copression
writeLines(ll, con <- bzfile(tf)); close(con)
file.info(tf)$size
stopifnot(identical(read.table(tf), morley))
## xz copression
writeLines(ll, con <- xzfile(tf, compression = -9)); close(con)
file.info(tf)$size
stopifnot(identical(read.table(tf), morley))
unlink(tf)


## weighted.mean with NAs (PR#14032)
x <- c(101, 102, NA)
stopifnot(all.equal(mean(x, na.rm = TRUE), weighted.mean(x, na.rm = TRUE)))
## divided by 3 in 2.10.0 (only)
## but *should* give NaN for empty:
stopifnot(identical(NaN, weighted.mean(0[0])),
	  identical(NaN, weighted.mean(NA,		na.rm=TRUE)),
	  identical(NaN, weighted.mean(rep(NA_real_,2), na.rm=TRUE)))
## all three gave 0  in 2.10.x and 2.11.x (but not previously)


## regexpr(fixed = TRUE) with a single-byte pattern matching to a MBCS string
x <- iconv("fa\xE7ile a ", "latin1", "UTF-8")
stopifnot(identical(regexpr(" ", x), regexpr(" ", x, fixed=TRUE)))
# fixed=TRUE reported match position in bytes in R <= 2.10.0
stopifnot(identical(regexpr(" a", x), regexpr(" a", x, fixed=TRUE)))
## always worked.

## unname() on 0-length vector
stopifnot(identical(1[FALSE], unname(c(a=1)[FALSE])))
## failed to drop names in 2.10.0


## complete.cases on 0-column data frame
complete.cases(data.frame(1:10)[-1])
## failed in 2.10.0


## PR#14035, converting (partially) unnamed lists to environments.
(qq <- with(list(2), ls()))
nchar(qq)
with(list(a=1, 2), ls())
## failed in R < 2.11.0


## chisq.test with over-long 'x' or 'y' arg
# https://stat.ethz.ch/pipermail/r-devel/2009-November/055700.html
x <- y <- rep(c(1000, 1001, 1002), each=5)
z <- eval(substitute(chisq.test(x,y), list(x=x)))
z
z$observed
## failed in 2.10.0


## unsplit(drop = TRUE) on a data frame failed (PR#14084)
dff <- data.frame(gr1 = factor(c(1,1,1,1,1,2,2,2,2,2,2), levels=1:4),
                  gr2 = factor(c(1,2,1,2,1,2,1,2,1,2,3), levels=1:4),
                  yy = rnorm(11), row.names = as.character(1:11))
dff2 <- split(dff, list(dff$gr1, dff$gr2), drop=TRUE)
dff3 <- unsplit(dff2, list(dff$gr1, dff$gr2), drop=TRUE)
stopifnot(identical(dff, dff3))
## failed in 2.10.0


## mean.difftime ignored its na.rm argument
z <- as.POSIXct(c("1980-01-01", "1980-02-01", NA, "1980-03-01", "1980-04-01"))
zz <- diff(z)
stopifnot(is.finite(mean(zz, na.rm=TRUE)))
## was NA in 2.10.0


## weighted means with zero weights and infinite values
x <- c(0, 1, 2, Inf)
w <- c(1, 1, 1, 0)
z <- weighted.mean(x, w)
stopifnot(is.finite(z))
## was NaN in 2.10.x


## Arithmetic operations involving "difftime"
z <- as.POSIXct(c("2009-12-01", "2009-12-02"), tz="UTC")
(zz <- z[2] - z[1])
(zzz <- z[1] + zz)
stopifnot(identical(zzz, z[2]),
          identical(zz + z[1], z[2]),
          identical(z[2] - zz, z[1]))
z <- as.Date(c("2009-12-01", "2009-12-02"))
(zz <- z[2] - z[1])
(zzz <- z[1] + zz)
stopifnot(identical(zzz, z[2]),
          identical(zz + z[1], z[2]),
          identical(z[2] - zz, z[1]))
## failed/gave wrong answers when Ops.difftime was introduced.


## quantiles, new possibilities in 2.11.0
x <- ordered(1:11, labels=letters[1:11])
quantile(x, type = 1)
quantile(x, type = 3)
st <- as.Date("1998-12-17")
en <- as.Date("2000-1-7")
ll <- seq(as.Date("2000-1-7"), as.Date("1997-12-17"), by="-1 month")
quantile(ll, type = 1)
quantile(ll, type = 3)
## failed prior to 2.11.0


## (asymptotic) point estimate in wilcox.test(*, conf.int=TRUE)
alt <- eval(formals(stats:::wilcox.test.default)$alternative)
Z <- c(-2, 0, 1, 1, 2, 2, 3, 5, 5, 5, 7)
E1 <- sapply(alt, function(a.)
	     wilcox.test(Z, conf.int = TRUE,
			 alternative = a., exact = FALSE)$estimate)
X <- c(6.5, 6.8, 7.1, 7.3, 10.2)
Y <- c(5.8, 5.8, 5.9, 6, 6, 6, 6.3, 6.3, 6.4, 6.5, 6.5)
E2 <- sapply(alt, function(a.)
	     wilcox.test(X,Y, conf.int = TRUE,
			 alternative = a., exact = FALSE)$estimate)
stopifnot(E1[-1] == E1[1],
	  E2[-1] == E2[1])
## was continiuity corrected, dependent on 'alternative', prior to 2.10.1


## read.table with embedded newlines in header (PR#14103)
writeLines(c('"B1', 'B2"', 'B3'), "test.dat")
z <- read.table("test.dat", header = TRUE)
unlink("test.dat")
stopifnot(identical(z, data.frame("B1.B2"="B3")))
## Left part of header to be read as data in R < 2.11.0

## switch() with  empty  '...'
stopifnot(is.null(switch("A")),
	  is.null(switch(1)), is.null(switch(3L)))
## the first one hung, 2nd gave error, in R <= 2.10.1


## factors with NA levels
V <- addNA(c(0,0,NA,0,1,1,0,NA,1,1))
stopifnot(identical(V, V[, drop = TRUE]))
stopifnot(identical(model.frame(~V), model.frame(~V, xlev = list(V=levels(V)))))
# dropped NA levels (in two places) in 2.10.1
V <- c(0,0,NA,0,1,1,0,NA,1,1)
stopifnot(identical(V, V[, drop = TRUE]))
stopifnot(identical(model.frame(~V), model.frame(~V, xlev = list(V=levels(V)))))
## check other cases have not been changed


## ks.test gave p=1 rather than p=0.9524 because abs(1/2-4/5)>3/10 was TRUE
stopifnot(all.equal(ks.test(1:5, c(2.5,4.5))$p.value, 20/21))


## NAs in utf8ToInt and v.v.
stopifnot(identical(utf8ToInt(NA_character_), NA_integer_),
          identical(intToUtf8(NA_integer_), NA_character_),
          identical(intToUtf8(NA_integer_, multiple = TRUE), NA_character_))
## no NA-handling prior to 2.11.0


## tcrossprod() for  matrix - vector combination
u <- 1:3 ; v <- 1:5
## would not work identically: names(u) <- LETTERS[seq_along(u)]
U <- as.matrix(u)
stopifnot(identical(tcrossprod(u,v), tcrossprod(U,v)),
	  identical(tcrossprod(u,v), u %*% t(v)),
	  identical(tcrossprod(v,u), tcrossprod(v,U)),
	  identical(tcrossprod(v,u), v %*% t(u)))
## tcrossprod(v,U) and (U,v) wrongly failed in R <= 2.10.1


## det() and determinant() in NA cases
m <- matrix(c(0, NA, 0, NA, NA, 0, 0, 0, 1), 3,3)
m0 <- rbind(0, cbind(0, m))
if(FALSE) { ## ideally, we'd want -- FIXME --
stopifnot(is.na(det(m)), 0 == det(m0))
} else print(c(det.m = det(m), det.m0 = det(m0)))
## the first wrongly gave 0  (still gives .. FIXME)


## c/rbind(deparse.level=2)
attach(mtcars)
(cn <- colnames(cbind(qsec, hp, disp)))
stopifnot(identical(cn, c("qsec", "hp", "disp")))
(cn <- colnames(cbind(qsec, hp, disp, deparse.level = 2)))
stopifnot(identical(cn, c("qsec", "hp", "disp")))
(cn <- colnames(cbind(qsec, log(hp), sqrt(disp))))
stopifnot(identical(cn, c("qsec", "", "")))
(cn <- colnames(cbind(qsec, log(hp), sqrt(disp), deparse.level = 2)))
stopifnot(identical(cn, c("qsec", "log(hp)", "sqrt(disp)")))
detach()
## 2.10.1 gave no column names for deparse.level=2


## Infinite-loops with match(incomparables=)
match(c("A", "B", "C"), "A", incomparables=NA)
match(c("A", "B", "C"), c("A", "B"), incomparables="A")
## infinite-looped in 2.10.1


## path.expand did not propagate NA
stopifnot(identical(c("foo", NA), path.expand(c("foo", NA))))
## 2.10.1 gave "NA"


## prettyNum(drop0trailing=TRUE) mangled complex values (PR#14201)
z <- c(1+2i, 1-3i)
str(z) # a user
stopifnot(identical(format(z, drop0trailing=TRUE), as.character(z)))
## 2.10.1 gave 'cplx [1:2] 1+2i 1+3i'


## "exact" fisher.test
dd <- data.frame(group=1, score=c(rep(0,14), rep(1,29), rep(2, 16)))[rep(1:59, 2),]
dd[,"group"] <- c(rep("DOG", 59), rep("kitty", 59))
Pv <- with(dd, fisher.test(score, group)$p.value)
stopifnot(0 <= Pv, Pv <= 1)
## gave P-value 1 + 1.17e-13  in R < 2.11.0


## Use of switch inside lapply (from BioC package ChromHeatMap)
lapply("forward", switch, forward = "posS", reverse = "negS")
## failed  when first converted to primitive.


## evaluation of arguments of log2
assertError(try(log2(quote(1:10))))
## 'worked' in 2.10.x by evaluting the arg twice.


## mean with NAs and trim (Bill Dunlap,
## https://stat.ethz.ch/pipermail/r-devel/2010-March/056982.html)
stopifnot(is.na(mean(c(1,10,100,NA), trim=0.1)),
          is.na(mean(c(1,10,100,NA), trim=0.26)))
## gave error, real value respectively in R <= 2.10.1


## all.equal(*, tol) for objects with numeric attributes
a <- structure(1:17, xtras = c(pi, exp(1)))
b <- a * (II <- (1 + 1e-7))
attr(b,"xtras") <- attr(a,"xtras") * II
stopifnot(all.equal(a,b, tol=2e-7))
## gave  "Attributes: .... relative difference: 1e-07"  in R <= 2.10.x


## Misuse of gzcon() [PR# 14237]
(ac <- getAllConnections())
tc <- textConnection("x", "w")
try(f <- gzcon(tc)) # -> error.. but did *damage* tc
newConn <- function(){ A <- getAllConnections(); A[is.na(match(A,ac))] }
(newC <- newConn())
gg <- tryCatch(getConnection(newC), error=identity)
stopifnot(identical(gg, tc))
close(tc)
stopifnot(length(newConn()) == 0)
## getConn..(*) seg.faulted in R <= 2.10.x


## splinefun(., method = "monoH.FC")
x <- 1:7 ; xx <- seq(0.9, 7.1, length=2^12)
y <- c(-12, -10, 3.5, 4.45, 4.5, 140, 142)
Smon <- splinefun(x, y, method = "monoH.FC")
stopifnot(0 <= min(Smon(xx, deriv=1)))
## slopes in [4.4, 4.66] were slightly negative, because m[] adjustments
## could be sightly off in cases of adjacency, for  R <= 2.11.0


## prettyDate( <Date> )
x <- as.Date("2008-04-22 09:45") + 0:5
px <- pretty(x, n = 5)
stopifnot(px[1] == "2008-04-22", length(px) == 6)
## did depend on the local timezone  at first


## cut( d, breaks = n) - for d of class  'Date' or 'POSIXt'
x <- seq(as.POSIXct("2000-01-01"), by = "days", length = 20)
stopifnot(nlevels(c1 <- cut(x, breaks = 3)) == 3,
	  nlevels(c2 <- cut(as.POSIXlt(x), breaks = 3)) == 3,
	  nlevels(c3 <- cut(as.Date(x), breaks = 3)) == 3,
	  identical(c1, c2))
## failed in R <= 2.11.0


## memDecompress (https://stat.ethz.ch/pipermail/r-devel/2010-May/057419.html)
char <- paste(replicate(200, "1234567890"), collapse="")
char.comp <- memCompress(char, type="xz")
char.dec <- memDecompress(char.comp, type="xz", asChar=TRUE)
stopifnot(nchar(char.dec) == nchar(char))
## short in R <= 2.11.0


## rbeta() with mass very close to 1 -- bug PR#14291
set.seed(1)
if(any(ii <- is.na(rbeta(5000, 100, 0.001))))
    stop("rbeta() gave NAs at ", paste(which(ii), collapse=", "),
         "\n")
## did give several, but platform dependently, in R <= 2.11.0


## print.ls_str() should not eval() some objects
E <- environment((function(miss)function(){})())
E$i <- 2:4
E$o <- as.name("foobar")
E$cl <- expression(sin(x))[[1]]
ls.str(E)
## 'o' failed in R <= 2.11.0 (others in earlier versions of R)


## print() {& str()} should distinguish named empty lists
stopifnot(identical("named list()",
		    capture.output(list(.=2)[0])))
## was just "list()" up to R <= 2.11.x


## stripchart with empty first level (PR#14317)
stripchart(decrease ~ treatment, data = OrchardSprays,
           subset = treatment != "A")
## failed in 2.11.1


## versions of pre-2.12.0 using zlib 1.2.[45] failed
zz <- gzfile("ex.gz", "w")  # compressed file
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep ="\n")
close(zz)
blah <- file("ex.gz", "r")
stopifnot(seek(blah) == 0)
## gave random large multiple of 2^32 on Linux systems attempting to
## use LFS support.


## pre-2.12.0 wrongly accessed 0-length entries
o0 <- as.octmode(integer(0))
stopifnot(identical(o0, o0 & "400"))
## gave a seg.fault at some point


## as.logical on factors
x <- factor(c("FALSE", "TRUE"))
stopifnot(identical(as.logical(x), c(FALSE, TRUE)))
# Lost documented behaviour when taken primitive in R 2.6.0
stopifnot(identical(as.vector(x, "logical"), c(FALSE, TRUE)))
# continued to work
## Reverted in 2.12.0.


## missing backquoting of default arguments in in prompt()
f <- function (FUN = `*`) {}
pr <- prompt(f, NA)$usage
stopifnot(identical(pr[2], "f(FUN = `*`)"))
## see https://stat.ethz.ch/pipermail/r-devel/2010-August/058126.html


## cut.POSIXt very near boundaries (PR#14351)
x <- as.POSIXlt("2010-08-10 00:00:01")
stopifnot(!is.na(cut(x, "5 hours")))
## was NA in 2.11.x


## summary() on data frames with invalid names -- in UTF-8 locale
DF <- data.frame(a = 1:3, b = 4:6)
nm <- names(DF) <- c("\xca", "\xcb")
cn <- gsub(" ", "", colnames(summary(DF)), useBytes = TRUE)
stopifnot(identical(cn, nm))
m <- as.matrix(DF)
DF <- data.frame(a = 1:3, m=I(m))
cn <- gsub(" ", "", colnames(summary(DF)), useBytes = TRUE)
stopifnot(identical(cn, c("a", paste("m.", nm, sep="", collapse=""))))
##  Had NAs in < 2.12.0


## [[<- could create invalid objects,
## https://stat.ethz.ch/pipermail/r-devel/2010-August/058312.html
z0 <- z <- factor(c("Two","Two","Three"), levels=c("One","Two","Three"))
z[[2]] <- "One"
stopifnot(typeof(z) == "integer")
z[[2]] <- "Two"
stopifnot(identical(z, z0))
## failed < 2.12.0


## predict.loess with NAs
cars.lo <- loess(dist ~ speed, cars)
res <- predict(cars.lo, data.frame(speed = c(5, NA, 25)))
stopifnot(length(res) == 3L, is.na(res[2]))
res <- predict(cars.lo, data.frame(speed = c(5, NA, 25)), se = TRUE)
stopifnot(length(res$fit) == 3L, is.na(res$fit[2]),
          length(res$se.fit) == 3L, is.na(res$se.fit[2]))
cars.lo2 <- loess(dist ~ speed, cars, control = loess.control(surface = "direct"))
res <- predict(cars.lo2, data.frame(speed = c(5, NA, 25)))
stopifnot(length(res) == 3L, is.na(res[2]))
res <- predict(cars.lo2, data.frame(speed = c(5, NA, 25)), se = TRUE)
stopifnot(length(res$fit) == 3L, is.na(res$fit[2]),
          length(res$se.fit) == 3L, is.na(res$se.fit[2]))
## Used na.omit prior to 2.12.0


## student typo
try( ksmooth(cars$speed, cars$dists) )
## now error about y (== NULL);  segfaulted <= 2.11.1


## do.call()ing NextMethod and empty args:
try( do.call(function(x) NextMethod('foo'),list()) )
## segfaulted <= 2.11.1


## identical() returned FALSE on external ptr with
## identical addresses <= 2.11.1
stopifnot(identical(
                    getNativeSymbolInfo("R_getSymbolInfo", "base"),
                    getNativeSymbolInfo("R_getSymbolInfo", "base")
                    ))
stopifnot(!identical(
                     getNativeSymbolInfo("R_getSymbolInfo", "base"),
                     getNativeSymbolInfo("R_getRegisteredRoutines", "base")
                     ))


## getNamespaceVersion() etc
stopifnot(getNamespaceVersion("stats") == getRversion())
## failed in R 2.11.x


## PR#14383
x <- rnorm(100)
z1 <- quantile(x, type = 6, probs = c(0, .5))
z2 <- quantile(x, type = 6, probs = c(.5, 0))
stopifnot(z1 == rev(z2))
## differed in 2.11.x



## found from fallback test in slam 0.1-15
## most likely indicates an inaedquate BLAS.
x <- matrix(c(1, 0, NA, 1), 2, 2)
y <- matrix(c(1, 0, 0, 2, 1, 0), 3, 2)
(z <- tcrossprod(x, y))
stopifnot(identical(z, x %*% t(y)))
stopifnot(is.nan(log(0) %*% 0))
## depended on the BLAS in use: some (including the reference BLAS)
## had z[1,3] == 0 and log(0) %*% 0 as as.matrix(0).


## PR#14393
f <- factor(c(NA, 1, 2), levels = 1:3, labels = 1:3)
mf <- model.frame(~ f, na.action = na.pass, drop.unused.levels = TRUE)
stopifnot(identical(mf$f, f[,drop=TRUE]))
## failed to drop < 2.12.0


## subassignment to expressions sometimes coerced them to lists.
x1 <- x2 <- x3 <- expression(a = pi, b = pi^2)
x1["b"] <- expression(pi^3)
stopifnot(is.expression(x1)) # OK
x1["a"] <- NULL
stopifnot(is.expression(x1))
x2[["b"]] <- quote(pi^3)
stopifnot(is.expression(x2)) # OK
x2[["a"]] <- NULL
stopifnot(is.expression(x2))
x3$a <- NULL
stopifnot(is.expression(x3))
## coerced to lists


proc.time()
