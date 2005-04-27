## Regression tests for which the printed output is the issue
### _and_ must work (no Recommended packages, please)

postscript("reg-tests-2.ps", encoding = "ISOLatin1.enc")
RNGversion("1.6.2")

### moved from various .Rd files
## abbreviate
for(m in 1:5) {
  cat("\n",m,":\n")
  print(as.vector(abbreviate(state.name, minl=m)))
}

## apply
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x,  2, summary) # 6 x n matrix
apply(x,  1, quantile)# 5 x n matrix

d.arr <- 2:5
arr <- array(1:prod(d.arr), d.arr,
         list(NULL,letters[1:d.arr[2]],NULL,paste("V",4+1:d.arr[4],sep="")))
aa <- array(1:20,c(2,2,5))
str(apply(aa[FALSE,,,drop=FALSE], 1, dim))# empty integer, `incorrect' dim.
stopifnot(
       apply(arr, 1:2, sum) == t(apply(arr, 2:1, sum)),
       aa == apply(aa,2:3,function(x) x),
       all.equal(apply(apply(aa,2:3, sum),2,sum),
                 10+16*0:4, tol=4*.Machine$double.eps)
)
marg <- list(1:2, 2:3, c(2,4), c(1,3), 2:4, 1:3, 1:4)
for(m in marg) print(apply(arr, print(m), sum))
for(m in marg) ## 75% of the time here was spent on the names
  print(dim(apply(arr, print(m), quantile, names=FALSE)) == c(5,d.arr[m]))

## Bessel
nus <- c(0:5,10,20)

x0 <- 2^(-20:10)
plot(x0,x0, log='xy', ylab="", ylim=c(.1,1e60),type='n',
     main = "Bessel Functions -Y_nu(x)  near 0\n log - log  scale")
for(nu in sort(c(nus,nus+.5))) lines(x0, -besselY(x0,nu=nu), col = nu+2)
legend(3,1e50, leg=paste("nu=", paste(nus,nus+.5, sep=",")), col=nus+2, lwd=1)

x <- seq(3,500);yl <- c(-.3, .2)
plot(x,x, ylim = yl, ylab="",type='n', main = "Bessel Functions  Y_nu(x)")
for(nu in nus){xx <- x[x > .6*nu]; lines(xx,besselY(xx,nu=nu), col = nu+2)}
legend(300,-.08, leg=paste("nu=",nus), col = nus+2, lwd=1)

x <- seq(10,50000,by=10);yl <- c(-.1, .1)
plot(x,x, ylim = yl, ylab="",type='n', main = "Bessel Functions  Y_nu(x)")
for(nu in nus){xx <- x[x > .6*nu]; lines(xx,besselY(xx,nu=nu), col = nu+2)}
summary(bY <- besselY(2,nu = nu <- seq(0,100,len=501)))
which(bY >= 0)
summary(bY <- besselY(2,nu = nu <- seq(3,300,len=51)))
summary(bI <- besselI(x = x <- 10:700, 1))
## end of moved from Bessel.Rd

## data.frame
set.seed(123)
L3 <- LETTERS[1:3]
str(d <- data.frame(cbind(x=1, y=1:10), fac=sample(L3, 10, repl=TRUE)))
(d0  <- d[, FALSE]) # NULL dataframe with 10 rows
(d.0 <- d[FALSE, ]) # <0 rows> dataframe  (3 cols)
(d00 <- d0[FALSE,])  # NULL dataframe with 0 rows
(d000 <- data.frame()) #but not quite the same as d00:
!identical(d00, d000)
dput(d00)
dput(d000)
stopifnot(identical(d, cbind(d, d0)),
          identical(d, cbind(d0, d)),
          identical(d, rbind(d,d.0)),
          identical(d, rbind(d.0,d)),
          identical(d, rbind(d00,d)),
          identical(d, rbind(d,d00)),

          TRUE )
## Comments: failed before ver. 1.4.0

## diag
diag(array(1:4, dim=5))
## test behaviour with 0 rows or columns
diag(0)
z <- matrix(0, 0, 4)
diag(z)
diag(z) <- numeric(0)
z
## end of moved from diag.Rd

## format
## handling of quotes
zz <- data.frame(a=I("abc"), b=I("def\"gh"))
format(zz)
## " (E fontification)

## printing more than 16 is platform-dependent
for(i in c(1:5,10,15,16)) cat(i,":\t",format(pi,digits=i),"\n")

p <- c(47,13,2,.1,.023,.0045, 1e-100)/1000
format.pval(p)
format.pval(p / 0.9)
format.pval(p / 0.9, dig=3)
## end of moved from format.Rd


## is.finite
x <- c(100,-1e-13,Inf,-Inf, NaN, pi, NA)
x #  1.000000 -3.000000       Inf      -Inf        NA  3.141593        NA
names(x) <- formatC(x, dig=3)
is.finite(x)
##-   100 -1e-13 Inf -Inf NaN 3.14 NA
##-     T      T   .    .   .    T  .
is.na(x)
##-   100 -1e-13 Inf -Inf NaN 3.14 NA
##-     .      .   .    .   T    .  T
which(is.na(x) & !is.nan(x))# only 'NA': 7

is.na(x) | is.finite(x)
##-   100 -1e-13 Inf -Inf NaN 3.14 NA
##-     T      T   .    .   T    T  T
is.infinite(x)
##-   100 -1e-13 Inf -Inf NaN 3.14 NA
##-     .      .   T    T   .    .  .

##-- either  finite or infinite  or  NA:
all(is.na(x) != is.finite(x) | is.infinite(x)) # TRUE
all(is.nan(x) != is.finite(x) | is.infinite(x)) # FALSE: have 'real' NA

##--- Integer
(ix <- structure(as.integer(x),names= names(x)))
##-   100 -1e-13    Inf   -Inf    NaN   3.14     NA
##-   100      0     NA     NA     NA      3     NA
all(is.na(ix) != is.finite(ix) | is.infinite(ix)) # TRUE (still)

storage.mode(ii <- -3:5)
storage.mode(zm <- outer(ii,ii, FUN="*"))# integer
storage.mode(zd <- outer(ii,ii, FUN="/"))# double
range(zd, na.rm=TRUE)# -Inf  Inf
zd[,ii==0]

(storage.mode(print(1:1 / 0:0)))# Inf "double"
(storage.mode(print(1:1 / 1:1)))# 1 "double"
(storage.mode(print(1:1 + 1:1)))# 2 "integer"
(storage.mode(print(2:2 * 2:2)))# 4 "integer"
## end of moved from is.finite.Rd


## kronecker
fred <- matrix(1:12, 3, 4, dimnames=list(LETTERS[1:3], LETTERS[4:7]))
bill <- c("happy" = 100, "sad" = 1000)
kronecker(fred, bill, make.dimnames = TRUE)

bill <- outer(bill, c("cat"=3, "dog"=4))
kronecker(fred, bill, make.dimnames = TRUE)

# dimnames are hard work: let's test them thoroughly

dimnames(bill) <- NULL
kronecker(fred, bill, make=TRUE)
kronecker(bill, fred, make=TRUE)

dim(bill) <- c(2, 2, 1)
dimnames(bill) <- list(c("happy", "sad"), NULL, "")
kronecker(fred, bill, make=TRUE)

bill <- array(1:24, c(3, 4, 2))
dimnames(bill) <- list(NULL, NULL, c("happy", "sad"))
kronecker(bill, fred, make=TRUE)
kronecker(fred, bill, make=TRUE)

fred <- outer(fred, c("frequentist"=4, "bayesian"=4000))
kronecker(fred, bill, make=TRUE)
## end of moved from kronecker.Rd

## merge
authors <- data.frame(
    surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
    nationality = c("US", "Australia", "US", "UK", "Australia"),
    deceased = c("yes", rep("no", 4)))
books <- data.frame(
    name = c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core"),
    title = c("Exploratory Data Analysis",
              "Modern Applied Statistics ...",
              "LISP-STAT",
              "Spatial Statistics", "Stochastic Simulation",
              "Interactive Data Analysis",
              "An Introduction to R"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA,
                     "Venables & Smith"))
b2 <- books; names(b2)[1] <- names(authors)[1]

merge(authors, b2, all.x = TRUE)
merge(authors, b2, all.y = TRUE)

## empty d.f. :
merge(authors, b2[7,])

merge(authors, b2[7,], all.y = TRUE)
merge(authors, b2[7,], all.x = TRUE)
## end of moved from merge.Rd

## NA
is.na(c(1,NA))
is.na(paste(c(1,NA)))
is.na(list())# logical(0)
ll <- list(pi,"C",NaN,Inf, 1:3, c(0,NA), NA)
is.na (ll)
is.nan(ll)
## end of moved from NA.Rd

## is.na was returning unset values on nested lists
ll <- list(list(1))
for (i in 1:5) print(as.integer(is.na(ll)))

## scale
## test out NA handling
tm <- matrix(c(2,1,0,1,0,NA,NA,NA,0), nrow=3)
scale(tm, , FALSE)
scale(tm)
## end of moved from scale.Rd

## tabulate
tabulate(numeric(0))
## end of moved from tabulate.Rd

## ts
# Ensure working arithmetic for `ts' objects :
stopifnot(z == z)
stopifnot(z-z == 0)

ts(1:5, start=2, end=4) # truncate
ts(1:5, start=3, end=17)# repeat
## end of moved from ts.Rd

### end of moved


## PR 715 (Printing list elements w/attributes)
##
l <- list(a=10)
attr(l$a, "xx") <- 23
l
## Comments:
## should print as
# $a:
# [1] 10
# attr($a, "xx"):
# [1] 23

## On the other hand
m <- matrix(c(1, 2, 3, 0, 10, NA), 3, 2)
na.omit(m)
## should print as
#      [,1] [,2]
# [1,]    1    0
# [2,]    2   10
# attr(,"na.action")
# [1] 3
# attr(,"na.action")
# [1] "omit"

## and
x <- 1
attr(x, "foo") <- list(a="a")
x
## should print as
# [1] 1
# attr(,"foo")
# attr(,"foo")$a
# [1] "a"


## PR 746 (printing of lists)
##
test.list <- list(A = list(formula=Y~X, subset=TRUE),
                  B = list(formula=Y~X, subset=TRUE))

test.list
## Comments:
## should print as
# $A
# $A$formula
# Y ~ X
#
# $A$subset
# [1] TRUE
#
#
# $B
# $B$formula
# Y ~ X
#
# $B$subset
# [1] TRUE

## Marc Feldesman 2001-Feb-01.  Precision in summary.data.frame & *.matrix
summary(attenu)
summary(attenu, digits = 5)
summary(data.matrix(attenu), digits = 5)# the same for matrix
## Comments:
## No difference between these in 1.2.1 and earlier
set.seed(1)
x <- c(round(runif(10), 2), 10000)
summary(x)
summary(data.frame(x))
## Comments:
## All entries show all 3 digits after the decimal point now.

## Chong Gu 2001-Feb-16.  step on binomials
"detg1" <-
structure(list(Temp = structure(c(2, 1, 2, 1, 2, 1, 2, 1, 2,
1, 2, 1), .Label = c("High", "Low"), class = "factor"), M.user = structure(c(1,
1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2), .Label = c("N", "Y"), class = "factor"),
    Soft = structure(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3), .Label = c("Hard",
    "Medium", "Soft"), class = "factor"), M = c(42, 30, 52, 43,
    50, 23, 55, 47, 53, 27, 49, 29), X = c(68, 42, 37, 24, 66,
    33, 47, 23, 63, 29, 57, 19)), .Names = c("Temp", "M.user",
"Soft", "M", "X"), class = "data.frame", row.names = c("1", "3",
"5", "7", "9", "11", "13", "15", "17", "19", "21", "23"))
detg1.m0 <- glm(cbind(X,M)~1,binomial,detg1)
detg1.m0
step(detg1.m0,scope=list(upper=~M.user*Temp*Soft))

## PR 829 (empty values in all.vars)
## This example by Uwe Ligges <ligges@statistik.uni-dortmund.de>

temp <- matrix(1:4, 2)
all.vars(temp ~ 3) # OK
all.vars(temp[1, ] ~ 3) # wrong in 1.2.1

## 2001-Feb-22 from David Scott.
## rank-deficient residuals in a manova model.
gofX.df<-
  structure(list(A = c(0.696706709347165, 0.362357754476673,
-0.0291995223012888,
0.696706709347165, 0.696706709347165, -0.0291995223012888, 0.696706709347165,
-0.0291995223012888, 0.362357754476673, 0.696706709347165, -0.0291995223012888,
0.362357754476673, -0.416146836547142, 0.362357754476673, 0.696706709347165,
0.696706709347165, 0.362357754476673, -0.416146836547142, -0.0291995223012888,
-0.416146836547142, 0.696706709347165, -0.416146836547142, 0.362357754476673,
-0.0291995223012888), B = c(0.717356090899523, 0.932039085967226,
0.999573603041505, 0.717356090899523, 0.717356090899523, 0.999573603041505,
0.717356090899523, 0.999573603041505, 0.932039085967226, 0.717356090899523,
0.999573603041505, 0.932039085967226, 0.909297426825682, 0.932039085967226,
0.717356090899523, 0.717356090899523, 0.932039085967226, 0.909297426825682,
0.999573603041505, 0.909297426825682, 0.717356090899523, 0.909297426825682,
0.932039085967226, 0.999573603041505), C = c(-0.0291995223012888,
-0.737393715541246, -0.998294775794753, -0.0291995223012888,
-0.0291995223012888, -0.998294775794753, -0.0291995223012888,
-0.998294775794753, -0.737393715541246, -0.0291995223012888,
-0.998294775794753, -0.737393715541246, -0.653643620863612, -0.737393715541246,
-0.0291995223012888, -0.0291995223012888, -0.737393715541246,
-0.653643620863612, -0.998294775794753, -0.653643620863612,
-0.0291995223012888,
-0.653643620863612, -0.737393715541246, -0.998294775794753),
    D = c(0.999573603041505, 0.67546318055115, -0.0583741434275801,
    0.999573603041505, 0.999573603041505, -0.0583741434275801,
    0.999573603041505, -0.0583741434275801, 0.67546318055115,
    0.999573603041505, -0.0583741434275801, 0.67546318055115,
    -0.756802495307928, 0.67546318055115, 0.999573603041505,
    0.999573603041505, 0.67546318055115, -0.756802495307928,
    -0.0583741434275801, -0.756802495307928, 0.999573603041505,
    -0.756802495307928, 0.67546318055115, -0.0583741434275801
    ), groups = structure(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2,
    2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3), class = "factor", .Label = c("1",
    "2", "3"))), .Names = c("A", "B", "C", "D", "groups"), row.names = c("1",
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
"14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"
), class = "data.frame")

gofX.manova <- manova(formula = cbind(A, B, C, D) ~ groups, data = gofX.df)
try(summary(gofX.manova))
## should fail with an error message `residuals have rank 3 < 4'

## Prior to 1.3.0 dist did not handle missing values, and the
## internal C code was incorrectly scaling for missing values.
z <- as.matrix(t(trees))
z[1,1] <- z[2,2] <- z[3,3] <- z[2,4] <- NA
dist(z, method="euclidean")
dist(z, method="maximum")
dist(z, method="manhattan")
dist(z, method="canberra")

## F. Tusell 2001-03-07.  printing kernels.
kernel("daniell", m=5)
kernel("modified.daniell", m=5)
kernel("daniell", m=c(3,5,7))
## fixed by patch from Adrian Trapletti 2001-03-08

## Start new year (i.e. line) at Jan:
(tt <- ts(1:10, start = c(1920,7), end = c(1921,4), freq = 12))
cbind(tt, tt + 1)


## PR 883 (cor(x,y) when is.null(y))
try(cov(rnorm(10), NULL))
try(cor(rnorm(10), NULL))
## gave the variance and 1 respectively in 1.2.2.
try(var(NULL))
try(var(numeric(0)))
## gave NA in 1.2.2


## PR 960 (format() of a character matrix converts to vector)
## example from <John.Peters@tip.csiro.au>
a <- matrix(c("axx","b","c","d","e","f","g","h"), nrow=2)
format(a)
format(a, justify="right")
## lost dimensions in 1.2.3


## PR 963
res <- svd(rbind(1:7))## $v lost dimensions in 1.2.3
if(res$u[1,1] < 0) {res$u <- -res$u; res$v <- -res$v}
res


## Make sure  on.exit() keeps being evaluated in the proper env [from PD]:
## A more complete example:
g1 <- function(fitted) { on.exit(remove(fitted)); return(function(foo) foo) }
g2 <- function(fitted) { on.exit(remove(fitted));        function(foo) foo }
f <- function(g) { fitted <- 1; h <- g(fitted); print(fitted)
                   ls(envir=environment(h)) }
f(g1)
f(g2)

f2 <- function()
{
  g.foo <- g1
  g.bar <- g2
  g <- function(x,...) UseMethod("g")
  fitted <- 1; class(fitted) <- "foo"
  h <- g(fitted); print(fitted); print(ls(envir=environment(h)))
  fitted <- 1; class(fitted) <- "bar"
  h <- g(fitted); print(fitted); print(ls(envir=environment(h)))
  invisible(NULL)
}
f2()
## The first case in f2() is broken in 1.3.0(-patched).

## on.exit() consistency check from Luke:
g <- function() as.environment(-1)
f <- function(x) UseMethod("f")
f.foo <- function(x) { on.exit(e <<- g()); NULL }
f.bar <- function(x) { on.exit(e <<- g()); return(NULL) }
f(structure(1,class = "foo"))
ls(env = e)# only "x", i.e. *not* the GlobalEnv
f(structure(1,class = "bar"))
stopifnot("x" == ls(env = e))# as above; wrongly was .GlobalEnv in R 1.3.x


## some tests that R supports logical variables in formulae
## it coerced them to numeric prior to 1.4.0
## they should appear like 2-level factors, following S

oldCon <- options("contrasts")
y <- rnorm(10)
x <- rep(c(TRUE, FALSE), 5)
model.matrix(y ~ x)
lm(y ~ x)
DF <- data.frame(x, y)
lm(y ~ x, data=DF)
options(contrasts=c("contr.helmert", "contr.poly"))
model.matrix(y ~ x)
lm(y ~ x, data=DF)
z <- 1:10
lm(y ~ x*z)
lm(y ~ x*z - 1)
options(oldCon)

## diffinv, Adrian Trapletti, 2001-08-27
x <- ts(1:10)
diffinv(diff(x),xi=x[1])
diffinv(diff(x,lag=1,differences=2),lag=1,differences=2,xi=x[1:2])
## last had wrong start and end

## PR#1072  (Reading Inf and NaN values)
as.numeric(as.character(NaN))
as.numeric(as.character(Inf))
## were NA on Windows at least under 1.3.0.

## PR#1092 (rowsum dimnames)
rowsum(matrix(1:12, 3,4), c("Y","X","Y"))
## rownames were 1,2 in <= 1.3.1.

## PR#1115 (saving strings with ascii=TRUE)
x <- y <- unlist(as.list(
    parse(text=paste("\"\\",
          as.character(structure(0:255,class="octmode")),
             "\"",sep=""))))
save(x, ascii=T, file=(fn <- tempfile()))
load(fn)
all(x==y)
unlink(fn)
## 1.3.1 had trouble with \


## Some tests of sink() and connections()
## capture all the output to a file.
zz <- file("all.Rout", open="wt")
sink(zz)
sink(zz, type="message")
try(log("a"))
## back to the console
sink(type="message")
sink()
try(log("a"))

## capture all the output to a file.
zz <- file("all.Rout", open="wt")
sink(zz)
sink(zz, type="message")
try(log("a"))

## bail out
closeAllConnections()
(foo <- showConnections())
stopifnot(nrow(foo) == 0)
try(log("a"))
unlink("all.Rout")
## many of these were untested before 1.4.0.


## test mean() works on logical but not factor
x <- c(TRUE, FALSE, TRUE, TRUE)
mean(x)
mean(as.factor(x))
## last had confusing error message in 1.3.1.


## Kurt Hornik 2001-Nov-13
z <- table(x = 1:2, y = 1:2)
z - 1
unclass(z - 1)
## lost object bit prior to 1.4.0, so printed class attribute.


## PR#1226  (predict.mlm ignored newdata)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
data <- data.frame(weight, group)
fit <- lm(cbind(w=weight, w2=weight^2) ~ group, data=data)
predict(fit, newdata=data[1:2, ])
## was 20 rows in R <= 1.4.0


## Chong Gu 2002-Feb-8: `.' not expanded in drop1
lab <- dimnames(HairEyeColor)
HairEye <- cbind(expand.grid(Hair=lab$Hair, Eye=lab$Eye, Sex=lab$Sex),
                 Fr=as.vector(HairEyeColor))
HairEye.fit <- glm(Fr ~ . ^2, poisson, HairEye)
drop1(HairEye.fit)
## broken around 1.2.1 it seems.


## PR#1329  (subscripting matrix lists)
m <- list(a1=1:3, a2=4:6, a3=pi, a4=c("a","b","c"))
dim(m) <- c(2,2)
m
m[,2]
m[2,2]
## 1.4.1 returned null components: the case was missing from a switch.

m <- list(a1=1:3, a2=4:6, a3=pi, a4=c("a","b","c"))
matrix(m, 2, 2)
## 1.4.1 gave `Unimplemented feature in copyVector'

x <- vector("list",6)
dim(x) <- c(2,3)
x[1,2] <- list(letters[10:11])
x
## 1.4.1 gave `incompatible types in subset assignment'


## printing of matrix lists
m <- list(as.integer(1), pi, 3+5i, "testit", TRUE, factor("foo"))
dim(m) <- c(1, 6)
m
## prior to 1.5.0 had quotes for 2D case (but not kD, k > 2),
## gave "numeric,1" etc, (even "numeric,1" for integers and factors)


## ensure RNG is unaltered.
for(type in c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
              "Mersenne-Twister", "Knuth-TAOCP", "Knuth-TAOCP-2002"))
{
    set.seed(123, type)
    print(RNGkind())
    runif(100); print(runif(4))
    set.seed(1000, type)
    runif(100); print(runif(4))
    set.seed(77, type)
    runif(100); print(runif(4))
}
RNGkind(normal.kind = "Kinderman-Ramage")
set.seed(123)
RNGkind()
rnorm(4)
RNGkind(normal.kind = "Ahrens-Dieter")
set.seed(123)
RNGkind()
rnorm(4)
RNGkind(normal.kind = "Box-Muller")
set.seed(123)
RNGkind()
rnorm(4)
set.seed(123)
runif(4)
set.seed(123, "default")
set.seed(123, "Marsaglia-Multicarry") ## Careful, not the default anymore
runif(4)
## last set.seed failed < 1.5.0.


## merging, ggrothendieck@yifan.net, 2002-03-16
d.df <- data.frame(x = 1:3, y = c("A","D","E"), z = c(6,9,10))
merge(d.df[1,], d.df)
## 1.4.1 got confused by inconsistencies in as.character


## PR#1394 (levels<-.factor)
f <- factor(c("a","b"))
levels(f) <- list(C="C", A="a", B="b")
f
## was  [1] C A; Levels:  C A  in 1.4.1


## PR#1408 Inconsistencies in sum()
x <- as.integer(2^30)
sum(x, x)    # did not warn in 1.4.1
sum(c(x, x)) # did warn
(z <- sum(x, x, 0.0)) # was NA in 1.4.1
typeof(z)


## NA levels in factors
(x <- factor(c("a", "NA", "b"), exclude=NULL))
## 1.4.1 had wrong order for levels
is.na(x)[3] <- TRUE
x
## missing entry prints as <NA>


## printing/formatting NA strings
(x <- c("a", "NA", NA, "b"))
print(x, quote = FALSE)
paste(x)
format(x)
format(x, justify = "right")
format(x, justify = "none")
## not ideal.


## print.ts problems  ggrothendieck@yifan.net on R-help, 2002-04-01
x <- 1:20
tt1 <- ts(x,start=c(1960,2), freq=12)
tt2 <- ts(10+x,start=c(1960,2), freq=12)
cbind(tt1, tt2)
## 1.4.1 had `Jan 1961' as `NA 1961'
## ...and 1.9.1 had it as `Jan 1960'!!

## glm boundary bugs (related to PR#1331)
x <- c(0.35, 0.64, 0.12, 1.66, 1.52, 0.23, -1.99, 0.42, 1.86, -0.02,
       -1.64, -0.46, -0.1, 1.25, 0.37, 0.31, 1.11, 1.65, 0.33, 0.89,
       -0.25, -0.87, -0.22, 0.71, -2.26, 0.77, -0.05, 0.32, -0.64, 0.39,
       0.19, -1.62, 0.37, 0.02, 0.97, -2.62, 0.15, 1.55, -1.41, -2.35,
       -0.43, 0.57, -0.66, -0.08, 0.02, 0.24, -0.33, -0.03, -1.13, 0.32,
       1.55, 2.13, -0.1, -0.32, -0.67, 1.44, 0.04, -1.1, -0.95, -0.19,
       -0.68, -0.43, -0.84, 0.69, -0.65, 0.71, 0.19, 0.45, 0.45, -1.19,
       1.3, 0.14, -0.36, -0.5, -0.47, -1.31, -1.02, 1.17, 1.51, -0.33,
       -0.01, -0.59, -0.28, -0.18, -1.07, 0.66, -0.71, 1.88, -0.14,
       -0.19, 0.84, 0.44, 1.33, -0.2, -0.45, 1.46, 1, -1.02, 0.68, 0.84)
y <- c(1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0,
       0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1,
       1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1,
       0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1,
       1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0)
try(glm(y ~ x, family = poisson(identity)))
## failed because start = NULL in 1.4.1
## now gives useful error message
glm(y ~ x, family = poisson(identity), start = c(1,0))
## step reduction failed in 1.4.1
set.seed(123)
y <- rpois(100, pmax(3*x, 0))
glm(y ~ x, family = poisson(identity), start = c(1,0))
warnings()


## extending char arrrays
x <- y <- LETTERS[1:2]
x[5] <- "C"
length(y) <- 5
x
y
## x was filled with "", y with NA in 1.5.0


## formula with no intercept, 2002-07-22
oldcon <- options(contrasts = c("contr.helmert", "contr.poly"))
U <- gl(3, 6, 18, labels=letters[1:3])
V <- gl(3, 2, 18, labels=letters[1:3])
A <- rep(c(0, 1), 9)
B <- rep(c(1, 0), 9)
set.seed(1); y <- rnorm(18)
terms(y ~ A:U + A:V - 1)
lm(y ~ A:U + A:V - 1)$coef  # 1.5.1 used dummies coding for V
lm(y ~ (A + B) : (U + V) - 1) # 1.5.1 used dummies coding for A:V but not B:V
options(oldcon)
## 1.5.1 miscomputed the first factor in the formula.


## quantile extremes, MM 13 Apr 2000 and PR#1852
for(k in 0:5)
    print(quantile(c(rep(-Inf,k+1), 0:k, rep(Inf, k)), pr=seq(0,1, .1)))
x <- c(-Inf, -Inf, Inf, Inf)
median(x)
quantile(x)
## 1.5.1 had -Inf not NaN in several places


## NAs in matrix dimnames
z <- matrix(1:9, 3, 3)
dimnames(z) <- list(c("x", "y", NA), c(1, NA, 3))
z
## NAs in dimnames misaligned when printing in 1.5.1


## weighted aov (PR#1930)
r <- c(10,23,23,26,17,5,53,55,32,46,10,8,10,8,23,0,3,22,15,32,3)
n <- c(39,62,81,51,39,6,74,72,51,79,13,16,30,28,45,4,12,41,30,51,7)
trt <- factor(rep(1:4,c(5,6,5,5)))
Y <- r/n
z <- aov(Y ~ trt, weights=n)
## 1.5.1 gave unweighted RSS


## rbind (PR#2266)
test <- as.data.frame(matrix(1:25, 5, 5))
test1 <- matrix(-(1:10), 2, 5)
rbind(test, test1)
rbind(test1, test)
## 1.6.1 treated matrix as a vector.


## escapes in non-quoted printing
x <- "\\abc\\"
names(x) <- 1
x
print(x, quote=FALSE)
## 1.6.2 had label misaligned


## summary on data frames containing data frames (PR#1891)
x <- data.frame(1:10)
x$z <- data.frame(x=1:10,yyy=11:20)
summary(x)
## 1.6.2 had NULL labels on output with z columns stacked.


## re-orderings in terms.formula (PR#2206)
form <- formula(y ~ a + b:c + d + e + e:d)
(tt <- terms(form))
(tt2 <- terms(formula(tt)))
stopifnot(identical(tt, tt2))
terms(delete.response(tt))
## both tt and tt2 re-ordered the formula < 1.7.0
## now try with a dot
terms(breaks ~ ., data = warpbreaks)
terms(breaks ~ . - tension, data = warpbreaks)
terms(breaks ~ . - tension, data = warpbreaks, simplify = TRUE)
terms(breaks ~ . ^2, data = warpbreaks)
terms(breaks ~ . ^2, data = warpbreaks, simplify = TRUE)
## 1.6.2 expanded these formulae out as in simplify = TRUE


## printing attributes (PR#2506)
(x <- structure(1:4, other=as.factor(LETTERS[1:3])))
## < 1.7.0 printed the codes of the factor attribute


## add logical matrix replacement indexing for data frames
TEMP <- data.frame(VAR1=c(1,2,3,4,5), VAR2=c(5,4,3,2,1), VAR3=c(1,1,1,1,NA))
TEMP[,c(1,3)][TEMP[,c(1,3)]==1 & !is.na(TEMP[,c(1,3)])] < -10
TEMP
##

## moved from reg-plot.R as exact output depends on rounding error
## PR 390 (axis for small ranges)

relrange <- function(x) {
    ## The relative range in EPS units
    r <- range(x)
    diff(r)/max(abs(r))/.Machine$double.eps
}

x <- c(0.12345678912345678,
       0.12345678912345679,
       0.12345678912345676)
relrange(x) ## 1.0125
plot(x) # `extra horizontal' ;  +- ok on Solaris; label off on Linux

y <- c(0.9999563255363383973418,
       0.9999563255363389524533,
       0.9999563255363382863194)
## The relative range number:
relrange(y) ## 3.000131
plot(y)# once gave infinite loop on Solaris [TL];  y-axis too long

## Comments: The whole issue was finally deferred to main/graphics.c l.1944
##    error("relative range of values is too small to compute accurately");
## which is not okay.

set.seed(101)
par(mfrow = c(3,3))
for(j.fac in 1e-12* c(10, 1, .7, .3, .2, .1, .05, .03, .01)) {
##           ====
    #set.seed(101) # or don't
    x <- pi + jitter(numeric(101), f = j.fac)
    rrtxt <- paste("rel.range =", formatC(relrange(x), dig = 4),"* EPS")
    cat("j.f = ", format(j.fac)," ;  ", rrtxt,"\n",sep="")
    plot(x, type = "l", main = rrtxt)
    cat("par(\"usr\")[3:4]:", formatC(par("usr")[3:4], wid = 10),"\n",
        "par(\"yaxp\") :   ", formatC(par("yaxp"), wid = 10),"\n\n", sep="")
}
par(mfrow = c(1,1))
## The warnings from inside GScale() will differ in their  relrange() ...
## >> do sloppy testing
## 2003-02-03 hopefully no more.  BDR
## end of PR 390


## print/show dispatch
hasMethods <- .isMethodsDispatchOn()
require(methods)
setClass("bar", representation(a="numeric"))
foo <- new("bar", a=pi)
foo
show(foo)
print(foo)

setMethod("show", "bar", function(object){cat("show method\n")})
show(foo)
foo
print(foo)
print(foo, digits = 4)

print.bar <- function(x, ...) cat("print method\n")
foo
print(foo)
show(foo)

setMethod("print", "bar", function(x, ...){cat("S4 print method\n")})
foo
print(foo)
show(foo)
print(foo, digits = 4)

setClassUnion("integer or NULL", members = c("integer","NULL"))
setClass("c1", representation(x = "integer", code = "integer or NULL"))
nc <- new("c1", x = 1:2)
str(nc)# gave ^ANULL^A in 2.0.0

if(!hasMethods) detach("package:methods")
##


## scoping rules calling step inside a function
"cement" <-
    structure(list(x1 = c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10),
                   x2 = c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68),
                   x3 = c(6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8),
                   x4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12),
                   y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5,
                   93.1, 115.9, 83.8, 113.3, 109.4)),
              .Names = c("x1", "x2", "x3", "x4", "y"), class = "data.frame",
              row.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
              "10", "11", "12", "13"))
teststep <- function(formula, data)
{
    d2 <- data
    fit <- lm(formula, data=d2)
    step(fit)
}
teststep(formula(y ~ .), cement)
## failed in 1.6.2

str(array(1))# not a scalar


## na.print="" shouldn't apply to (dim)names!
(tf <- table(ff <- factor(c(1:2,NA,2), exclude=NULL)))
identical(levels(ff), dimnames(tf)[[1]])
str(levels(ff))
## not quite ok previous to 1.7.0


## PR#3058  printing with na.print and right=TRUE
a <- matrix( c(NA, "a", "b", "10",
               NA, NA,  "d", "12",
               NA, NA,  NA,  "14"),
            byrow=T, ncol=4 )
print(a, right=TRUE, na.print=" ")
print(a, right=TRUE, na.print="----")
## misaligned in 1.7.0


## assigning factors to dimnames
A <- matrix(1:4, 2)
aa <- factor(letters[1:2])
dimnames(A) <- list(aa, NULL)
A
dimnames(A)
## 1.7.0 gave internal codes as display and dimnames()
## 1.7.1beta gave NAs via dimnames()
## 1.8.0 converts factors to character


## wishlist PR#2776: aliased coefs in lm/glm
set.seed(123)
x2 <- x1 <- 1:10
x3 <- 0.1*(1:10)^2
y <- x1 + rnorm(10)
(fit <- lm(y ~ x1 + x2 + x3))
summary(fit, cor = TRUE)
(fit <- glm(y ~ x1 + x2 + x3))
summary(fit, cor = TRUE)
## omitted silently in summary.glm < 1.8.0


## list-like indexing of data frames with drop specified
women["height"]
women["height", drop = FALSE]  # same with a warning
women["height", drop = TRUE]   # ditto
women[,"height", drop = FALSE] # no warning
women[,"height", drop = TRUE]  # a vector
## second and third were interpreted as women["height", , drop] in 1.7.x


## make.names
make.names("")
make.names(".aa")
## was "X.aa" in 1.7.1
make.names(".2")
make.names(".2a") # not valid in R
make.names(as.character(NA))
##


## strange names in data frames
as.data.frame(list(row.names=17))  # 0 rows in 1.7.1
aa <- data.frame(aa=1:3)
aa[["row.names"]] <- 4:6
aa # fine in 1.7.1
A <- matrix(4:9, 3, 2)
colnames(A) <- letters[1:2]
aa[["row.names"]] <- A
aa
## wrong printed names in 1.7.1

## assigning to NULL
a <- NULL
a[["a"]] <- 1
a
a <- NULL
a[["a"]] <- "something"
a
a <- NULL
a[["a"]] <- 1:3
a
## Last was an error in 1.7.1


## examples of 0-rank models, some empty, some rank-deficient
y <- rnorm(10)
x <- rep(0, 10)
(fit <- lm(y ~ 0))
summary(fit)
anova(fit)
predict(fit)
predict(fit, data.frame(x=x), se=TRUE)
predict(fit, type="terms", se=TRUE)
variable.names(fit) #should be empty
model.matrix(fit)

(fit <- lm(y ~ x + 0))
summary(fit)
anova(fit)
predict(fit)
predict(fit, data.frame(x=x), se=TRUE)
predict(fit, type="terms", se=TRUE)
variable.names(fit) #should be empty
model.matrix(fit)

(fit <- glm(y ~ 0))
summary(fit)
anova(fit)
predict(fit)
predict(fit, data.frame(x=x), se=TRUE)
predict(fit, type="terms", se=TRUE)

(fit <- glm(y ~ x + 0))
summary(fit)
anova(fit)
predict(fit)
predict(fit, data.frame(x=x), se=TRUE)
predict(fit, type="terms", se=TRUE)
## Lots of problems in 1.7.x


## lm.influence on deficient lm models
dat <- data.frame(y=rnorm(10), x1=1:10, x2=1:10, x3 = 0, wt=c(0,rep(1, 9)),
                  row.names=letters[1:10])
dat[3, 1] <- dat[4, 2] <- NA
lm.influence(lm(y ~ x1 + x2, data=dat, weights=wt, na.action=na.omit))
lm.influence(lm(y ~ x1 + x2, data=dat, weights=wt, na.action=na.exclude))
lm.influence(lm(y ~ 0, data=dat, weights=wt, na.action=na.omit))
lm.influence(lm(y ~ 0, data=dat, weights=wt, na.action=na.exclude))
lm.influence(lm(y ~ 0 + x3, data=dat, weights=wt, na.action=na.omit))
lm.influence(lm(y ~ 0 + x3, data=dat, weights=wt, na.action=na.exclude))
lm.influence(lm(y ~ 0, data=dat, na.action=na.exclude))
## last three misbehaved in 1.7.x, none had proper names.


## length of results in ARMAacf when lag.max is used
ARMAacf(ar=c(1.3,-0.6, -0.2, 0.1),lag.max=1) # was 4 in 1.7.1
ARMAacf(ar=c(1.3,-0.6, -0.2, 0.1),lag.max=2)
ARMAacf(ar=c(1.3,-0.6, -0.2, 0.1),lag.max=3)
ARMAacf(ar=c(1.3,-0.6, -0.2, 0.1),lag.max=4)
ARMAacf(ar=c(1.3,-0.6, -0.2, 0.1),lag.max=5) # failed in 1.7.1
ARMAacf(ar=c(1.3,-0.6, -0.2, 0.1),lag.max=6)
ARMAacf(ar=c(1.3,-0.6, -0.2, 0.1),lag.max=10)
##


## Indexing non-existent columns in a data frame
x <- data.frame(a = 1, b = 2)
try(x[c("a", "c")])
try(x[, c("a", "c")])
try(x[1, c("a", "c")])
## Second succeeded, third gave uniformative error message in 1.7.x.


## methods(class = ) with namespaces, .Primitives etc (many missing in 1.7.x):
meth2gen <- function(cl)
    noquote(sub(paste("\\.",cl,"$",sep=""),"", methods(class = cl)))
meth2gen("data.frame")
meth2gen("dendrogram")
## --> the output may need somewhat frequent updating..


## subsetting a 1D array lost the dimensions
x <- array(1:5, dim=c(5))
dim(x)
dim(x[, drop=TRUE])
dim(x[2:3])
dim(x[2])
dim(x[2, drop=FALSE])
dimnames(x) <- list(some=letters[1:5])
x[]
x[2:3]
x[2]
x[2, drop=FALSE]
## both dim and dimnames lost in 1.8.0


## print.dist() didn't show NA's prior to 1.8.1
x <- cbind(c(1,NA,2,3), c(NA,2,NA,1))
(d <- dist(x))
print(d, diag = TRUE)
##


## offsets in model terms where sometimes not deleted correctly
attributes(terms(~ a + b + a:b + offset(c)))[c("offset", "term.labels")]
attributes(terms(y ~ a + b + a:b + offset(c)))[c("offset", "term.labels")]
attributes(terms(~ offset(c) + a + b + a:b))[c("offset", "term.labels")]
attributes(terms(y ~ offset(c) + a + b + a:b))[c("offset", "term.labels")]
## errors prior to 1.8.1


## 0-level factors gave nonsensical answers in model.matrix
m <- model.frame(~x, data.frame(x=NA), na.action=na.pass)
model.matrix(~x, m)
lm.fit <- lm(y ~ x, data.frame(x=1:10, y=1:10))
try(predict(lm.fit, data.frame(x=NA)))
## wrong answers in 1.8.0, refused to run in 1.8.1



## failure to print data frame containing arrays
## raised by John Fox on R-devel on 2004-01-08
y1 <- array(1:10, dim=10)
y2 <- array(1:30, dim=c(10,3), dimnames=list(NULL, letters[1:3]))
y3 <- array(1:40, dim=c(10,2,2),
            dimnames=list(NULL, letters[1:2], NULL))
data.frame(y=y1)
data.frame(y=y2)
data.frame(y=y3)

as.data.frame(y1)
as.data.frame(y2)
as.data.frame(y3)

X <- data.frame(x=1:10)
X$y <- y1
X
sapply(X, dim)

X$y <- y2
X
sapply(X, dim)

X$y <- y3
X
sapply(X, dim)
## The last one fails in S.

## test of user hooks
for(id in c("A", "B")) {
    eval(substitute(
    {
setHook(packageEvent("stats4", "onLoad"),
        function(pkgname, ...) cat("onLoad", sQuote(pkgname), id, "\n"));
setHook(packageEvent("stats4", "attach"),
        function(pkgname, ...) cat("attach", sQuote(pkgname), id, "\n"));
setHook(packageEvent("stats4", "detach"),
        function(pkgname, ...) cat("detach", sQuote(pkgname), id, "\n"));
setHook(packageEvent("stats4", "onUnload"),
        function(pkgname, ...) cat("onUnload", sQuote(pkgname), id, "\n"))
    },
                    list(id=id)))
}
loadNamespace("stats4")
library("stats4")
detach("package:stats4")
unloadNamespace("stats4")
## Just tests


## rep(0-length-vector, length.out > 0)
rep(integer(0), length.out=0)
rep(integer(0), length.out=10)
typeof(.Last.value)
rep(logical(0), length.out=0)
rep(logical(0), length.out=10)
typeof(.Last.value)
rep(numeric(0), length.out=0)
rep(numeric(0), length.out=10)
typeof(.Last.value)
rep(character(0), length.out=0)
rep(character(0), length.out=10)
typeof(.Last.value)
rep(complex(0), length.out=0)
rep(complex(0), length.out=10)
typeof(.Last.value)
rep(list(), length.out=0)
rep(list(), length.out=10)
## always 0-length before 1.9.0


## supplying 0-length data to array and matrix
array(numeric(0), c(2, 2))
array(list(), c(2,2))
# worked < 1.8.0, error in 1.8.x
matrix(character(0), 1, 2)
matrix(integer(0), 1, 2)
matrix(logical(0), 1, 2)
matrix(numeric(0), 1, 2)
matrix(complex(0), 1, 2)
matrix(list(), 1, 2)
## did not work < 1.9.0


## S compatibility change in 1.9.0
rep(1:2, each=3, length=12)
## used to pad with NAs.


## PR#6510: aov() with error and -1
set.seed(1)
test.df <- data.frame (y=rnorm(8), a=gl(2,1,8), b=gl(2,3,8),c=gl(2,4,8))
aov(y ~ a + b + Error(c), data=test.df)
aov(y ~ a + b - 1 + Error(c), data=test.df)
## wrong assignment to strata labels < 1.9.0
## Note this is unbalanced and not a good example

binom.test(c(800,10))# p-value < epsilon


## Misleading error messages on integer overflow
## Uwe Ligges, R-devel, 2004-02-19
try(numeric(2^31))
try(matrix( , 2^31, 1))
try(matrix( , 2^31/10, 100))
try(array(dim=c(2^31/10, 100)))
## reported negative values (really integer NA) for R < 1.9.0


## aov with a singular error model
rd <- c(16.53, 12.12, 10.04, 15.32, 12.33, 10.1, 17.09, 11.69, 11.81, 14.75,
        10.72, 8.79, 13.14, 9.79, 8.36, 15.62, 9.64, 8.72, 15.32,
        11.35, 8.52, 13.27, 9.74, 8.78, 13.16, 10.16, 8.4, 13.08, 9.66,
        8.16, 12.17, 9.13, 7.43, 13.28, 9.16, 7.92, 118.77, 78.83, 62.2,
        107.29, 73.79, 58.59, 118.9, 66.35, 53.12, 372.62, 245.39, 223.72,
        326.03, 232.67, 209.44, 297.55, 239.71, 223.8)
sample.df <- data.frame(dep.variable=rd,
                        subject=factor(rep(paste("subj",1:6, sep=""),each=9)),
                        f1=factor(rep(rep(c("f1","f2","f3"),each=6),3)),
                        f2=factor(rep(c("g1","g2","g3"),each=18))
)
sample.aov <- aov(dep.variable ~ f1 * f2 + Error(subject/(f1+f2)), data=sample.df)
sample.aov
summary(sample.aov)
sample.aov <- aov(dep.variable ~ f1 * f2 + Error(subject/(f2+f1)), data=sample.df)
sample.aov
summary(sample.aov)
## failed in 1.8.1


## PR#6645  stem() with near-constant values
stem(rep(1, 100))
stem(rep(0.1, 10))
stem(c(rep(1, 10), 1+1.e-8))
stem(c(rep(1, 10), 1+1.e-9))
stem(c(rep(1, 10), 1+1.e-10), atom=0) # integer-overflow is avoided.
##  had integer overflows in 1.8.1, and silly shifts of decimal point


## PR#6633 warnings with vector op matrix, and more
set.seed(1)
x1 <- rnorm(3)
y1 <- rnorm(4)
x1 * y1
x1 * as.matrix(y1) # no warning in 1.8.1
x1 * matrix(y1,2,2)# ditto
z1 <- x1 > 0
z2 <- y1 > 0
z1 & z2
z1 & as.matrix(z2) # no warning in 1.8.1
x1 < y1            # no warning in 1.8.1
x1 < as.matrix(y1) # ditto
##


## summary method for mle
library(stats4)
N <- c(rep(3:6, 3), 7,7, rep(8,6), 9,9, 10,12)# sample from Pois(lam = 7)
summary(mle(function(Lam = 1) -sum(dpois(N, Lam))))
## "Coefficients" was "NULL" in 1.9.0's "devel"


## PR#6656 terms.formula(simplify = TRUE) was losing offset terms
## successive offsets caused problems
df <- data.frame(x=1:4, y=sqrt( 1:4), z=c(2:4,1))
fit1 <- glm(y ~ offset(x) + z, data=df)
update(fit1, ". ~.")$call
## lost offset in 1.7.0 to 1.8.1
terms(y ~ offset(x) + offset(log(x)) + z, data=df)
## failed to remove second offset from formula in 1.8.1
terms(y ~ offset(x) + z - z, data=df, simplify = TRUE)
## first fix failed for models with no non-offset terms.


## only the first two were wrong up to 1.8.1:
3:4 * 1e-100
8:11* 1e-100
1:2 * 1e-99
1:2 * 1e+99
8:11* 1e+99
3:4 * 1e+100
##


## negative subscripts could be mixed with NAs
x <- 1:3
try(x[-c(1, NA)])
## worked on some platforms, segfaulted on others in 1.8.1


## vector 'border' (and no 'pch', 'cex' nor 'bg'):
boxplot(count ~ spray, data = InsectSprays, border=2:7)
## gave warnings in 1.9.0

summary(as.Date(paste("2002-12", 26:31, sep="-")))
## printed all "2002.-12-29" in 1.9.1 {because digits was too small}
as.matrix(data.frame(d = as.POSIXct("2004-07-20")))
## gave a warning in 1.9.1


## Dump should quote when necessary (PR#6857)
x <- quote(b)
dump("x", "")
## doesn't quote b in 1.9.0


## some checks of indexing by character, used to test hashing code
x <- 1:26
names(x) <- letters
x[c("a", "aa", "aa")] <- 100:102
x

x <- 1:26
names(x) <- rep("", 26)
x[c("a", "aa", "aa")] <- 100:102
x
##


## tests of raw type
# tests of logic operators
x <- "A test string"
(y <- charToRaw(x))
(xx <- c(y, as.raw(0), charToRaw("more")))

!y
y & as.raw(15)
y | as.raw(128)

# tests of binary read/write
zz <- file("testbin", "wb")
writeBin(xx, zz)
close(zz)
zz <- file("testbin", "rb")
(yy <- readBin(zz, "raw", 100))
seek(zz, 0, "start")
readBin(zz, "integer", n=100, size = 1) # read as small integers
seek(zz, 0, "start")
readBin(zz, "character", 100)  # is confused by embedded nul.
seek(zz, 0, "start")
readChar(zz, length(xx)) # correct
seek(zz) # make sure current position is reported properly
close(zz)
unlink("testbin")

# tests of ASCII read/write.
cat(xx, file="testascii")
scan("testascii", what=raw(0))
unlink("testascii")
##


## Example of prediction not from newdata as intended.
set.seed(1)
y <- rnorm(10)
x  <- cbind(1:10, sample(1:10)) # matrix
xt <- cbind(1:2,  3:4)
(lm1 <- lm(y ~ x))
predict(lm1, newdata = data.frame(x= xt))
## warns as from 2.0.0


## eval could alter a data.frame/list second argument
data(trees)
a <- trees
eval(quote({Girth[1]<-NA;Girth}),a)
a[1, ]
trees[1, ]
## both a and trees got altered in 1.9.1


## write.table did not apply qmethod to col.names (PR#7171)
x <- data.frame("test string with \"" = c("a \" and a '"), check.names=FALSE)
write.table(x)
write.table(x, qmethod = "double")
## Quote in col name was unescaped in 1.9.1.


## extensions to read.table
Mat <- matrix(c(1:3, letters[1:3], 1:3, LETTERS[1:3],
                c("2004-01-01", "2004-02-01", "2004-03-01"),
                c("2004-01-01 12:00", "2004-02-01 12:00", "2004-03-01 12:00")),
              3, 6)
foo <- tempfile()
write.table(Mat, foo, col.names = FALSE, row.names = FALSE)
read.table(foo, colClasses = c(NA, NA, "NULL", "character", "Date", "POSIXct"))
unlist(sapply(.Last.value, class))
read.table(foo, colClasses = c("factor",NA,"NULL","factor","Date","POSIXct"))
unlist(sapply(.Last.value, class))
read.table(foo, colClasses = c(V4="character"))
unlist(sapply(.Last.value, class))
unlink(foo)
## added in 2.0.0


## write.table with complex columns (PR#7260, in part)
write.table(data.frame(x = 0.5+1:4, y = 1:4 + 1.5i), file = "")
# printed all as complex in 2.0.0.
write.table(data.frame(x = 0.5+1:4, y = 1:4 + 1.5i), file = "", dec=",")
## used '.' not ',' in 2.0.0

## splinefun() value test
(x <- seq(0,6, length=25))
mx <- sapply(c("fmm", "nat", "per"),
             function(m) splinefun(1:5, c(1,2,4,3,1), method = m)(x))
cbind(x,mx)


## infinite loop in read.fwf (PR#7350)
cat(file="test.txt", sep = "\n", "# comment 1", "1234567   # comment 2",
    "1 234567  # comment 3", "12345  67 # comment 4", "# comment 5")
read.fwf("test.txt", width=c(2,2,3), skip=1, n=4) # looped
read.fwf("test.txt", width=c(2,2,3), skip=1)      # 1 line short
read.fwf("test.txt", width=c(2,2,3), skip=0)
unlink("test.txt")
##


## split was not handling lists and raws
split(as.list(1:3), c(1,1,2))
(y <- charToRaw("A test string"))
(z <- split(y, rep(1:5, times=c(1,1,4,1,6))))
sapply(z, rawToChar)
## wrong results in 2.0.0


## tests of changed S3 implicit classes in 2.1.0
foo <- function(x, ...) UseMethod("foo")
foo.numeric <- function(x) cat("numeric arg\n")
foo(1:10)
foo(pi)
foo(matrix(1:10, 2, 5))
foo.integer <- function(x) cat("integer arg\n")
foo.double <- function(x) cat("double arg\n")
foo(1:10)
foo(pi)
foo(matrix(1:10, 2, 5))
##


## str() interpreted escape sequences prior to 2.1.0
x <- "ab\bc\ndef"
str(x)
str(x, vec.len=0)# failed in rev 32244
str(factor(x))

x <- c("a", NA, "b")
factor(x)
factor(x, exclude="")
str(x)
str(factor(x))
str(factor(x, exclude=""))
##


## print.factor(quote=TRUE) was not quoting levels
x <- c("a", NA, "b", 'a " test')
factor(x)
factor(x, exclude="")
print(factor(x), quote=TRUE)
print(factor(x, exclude=""), quote=TRUE)
## last two printed levels differently from values in 2.0.1


## write.table in marginal cases
x <- matrix(, 3, 0)
write.table(x) # 3 rows
write.table(x, row.names=FALSE)
# note: scan and read.table won't read this as they take empty fields as NA
## was 1 row in 2.0.1


## More tests of write.table
x <- list(a=1, b=1:2, c=3:4, d=5)
dim(x) <- c(2,2)
x
write.table(x)

x1 <- data.frame(a=1:2, b=I(matrix(LETTERS[1:4], 2, 2)), c = c("(i)", "(ii)"))
x1
write.table(x1) # In 2.0.1 had 3 headers, 4 cols
write.table(x1, quote=c(2,3,4))

x2 <- data.frame(a=1:2, b=I(list(a=1, b=2)))
x2
write.table(x2)

x3 <- seq(as.Date("2005-01-01"), len=6, by="day")
x4 <- data.frame(x=1:6, y=x3)
dim(x3) <- c(2,3)
x3
write.table(x3) # matrix, so loses class
x4
write.table(x4) # preserves class, does not quote
##


## Problem with earlier regexp code spotted by KH
grep("(.*s){2}", "Arkansas", v = TRUE)
grep("(.*s){3}", "Arkansas", v = TRUE)
grep("(.*s){3}", state.name, v = TRUE)
## Thought Arkansas had 3 s's.


## Replacing part of a non-existent column could create a short column.
xx<- data.frame(a=1:4, b=letters[1:4])
xx[2:3, "c"] <- 2:3
## gave short column in R < 2.1.0.


## add1/drop1 could give misleading results if missing values were involved
y <- rnorm(1:20)
x <- 1:20; x[10] <- NA
x2 <- runif(20); x2[20] <- NA
fit <- lm(y ~ x)
drop1(fit)
res <-  try(stats:::drop1.default(fit))
stopifnot(inherits(res, "try-error"))
add1(fit, ~ . +x2)
res <-  try(stats:::add1.default(fit, ~ . +x2))
stopifnot(inherits(res, "try-error"))
## 2.0.1 ran and gave incorrect answers.


## (PR#7789) escaped quotes in the first five lines for read.table
tf <- tempfile()
x <- c("6 'TV2  Shortland Street'",
       "2 'I don\\\'t watch TV at 7'",
       "1 'I\\\'m not bothered, whatever that looks good'",
       "2 'I channel surf'")
writeLines(x, tf)
read.table(tf)
x <- c("6 'TV2  Shortland Street'",
       "2 'I don''t watch TV at 7'",
       "1 'I''m not bothered, whatever that looks good'",
       "2 'I channel surf'")
writeLines(x, tf)
read.table(tf, sep=" ")
unlink(tf)
## mangled in 2.0.1


## (PR#7802) printCoefmat(signif.legend =FALSE) failed
set.seed(123)
cmat <- cbind(rnorm(3, 10), sqrt(rchisq(3, 12)))
cmat <- cbind(cmat, cmat[,1]/cmat[,2])
cmat <- cbind(cmat, 2*pnorm(-cmat[,3]))
colnames(cmat) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
printCoefmat(cmat, signif.stars = TRUE)
printCoefmat(cmat, signif.stars = TRUE, signif.legend = FALSE)
# no stars, so no legend
printCoefmat(cmat, signif.stars = FALSE)
printCoefmat(cmat, signif.stars = TRUE, signif.legend = TRUE)
## did not work in 2.1.0
