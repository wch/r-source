### Regression tests for which the printed output is the issue

postscript("reg-tests-2.ps")
RNGversion("1.6.2")

### moved from various .Rd files
## abbreviate
data(state)
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
data(attenu)
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
data(trees)
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
data(HairEyeColor)
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
data(warpbreaks)
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
if(!hasMethods) detach("package:methods")
##


## scoping rules calling step inside a function
if(require(MASS)) { # only for "cement"
    teststep <- function(formula, data)
    {
        d2 <- data
        fit <- lm(formula, data=d2)
        step(fit)
    }
    teststep(formula(y ~ .), cement)
    detach("package:MASS")
}
## failed in 1.6.2

str(array(1))# not a scalar

## na.print="" shouldn't apply to (dim)names!
(tf <- table(ff <- factor(c(1:2,NA,2), exclude=NULL)))
identical(levels(ff), dimnames(tf)[[1]])
str(levels(ff))
## not quite ok previous to 1.7.0

## str() for character & factors with NA (levels), and for Surv objects:
ff <- factor(c(2:1,  NA),  exclude = NULL)
str(levels(ff))
str(ff)
str(ordered(ff, exclude=NULL))
if(require(survival)) {
    data(aml)
    (sa <- Surv(aml$time, aml$status))
    str(sa)
    detach("package:survival")
}
## were different, the last one failed in 1.6.2 (at least)
