### Regression tests for which the printed output is the issue

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
# attr(,"na.action")attr(,"class")
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
library(mva)
data(trees)
z <- as.matrix(t(trees))
z[1,1] <- z[2,2] <- z[3,3] <- z[2,4] <- NA
dist(z, method="euclidean")
dist(z, method="maximum")
dist(z, method="manhattan")
dist(z, method="canberra")
detach("package:mva")

## F. Tusell 2001-03-07.  printing kernels.
library(ts)
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
svd(rbind(1:7))## $v lost dimensions in 1.2.3


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
library(ts)
x <- ts(1:10)
diffinv(diff(x),xi=x[1])
diffinv(diff(x,lag=1,differences=2),lag=1,differences=2,xi=x[1:2])
## last had wrong start and end
detach("package:ts")

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

