####  "All the examples" from  ./R-intro.texi
####  -- in a way that this should be(come) an executable script.

options(digits=5, width=65)##--- for outputs !

## 2. Simple Manipulations

x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
assign("x", c(10.4, 5.6, 3.1, 6.4, 21.7))
c(10.4, 5.6, 3.1, 6.4, 21.7) -> x
.Last.value
1/x

y <- c(x, 0, x)
v <- 2*x + y + 1
##-  Warning message:
##-  longer object length
##-  is not a multiple of shorter object length in: 2 * x + y

sqrt(-17)
##- [1] NaN
##- Warning message:
##- NaNs produced in: sqrt(-17)

sqrt(-17+0i)

###--  2.3 .. regular sequences

1:30

n <- 10

1:n-1
1:(n-1)
30:1

seq(2,10)
all(seq(1,30) == seq(to=30, from=1))

seq(-5, 5, by=.2) -> s3
s4 <- seq(length=51, from=-5, by=.2)
all.equal(s3,s4)

s5 <- rep(x, times=5)
s6 <- rep(x, each=5)

temp <- x > 13

z <- c(1:3,NA);  ind <- is.na(z)

0/0
Inf - Inf

labs <- paste(c("X","Y"), 1:10, sep="")
labs

x <- c(z,z-2)#-- NOT in texi ; more interesting
y <- x[!is.na(x)]

(x+1)[(!is.na(x)) & x>0] -> z
z

x <- c(x, 9:12)# long enough:
x[1:10]

c("x","y")[rep(c(1,2,2,1), times=4)]

y <- x[-(1:5)]
y

fruit <- c(5, 10, 1, 20)
names(fruit) <- c("orange", "banana", "apple", "peach")
 fruit

lunch <- fruit[c("apple","orange")]
 lunch

 x
x[is.na(x)] <- 0
 x

y <- -4:9
y[y < 0] <- -y[y < 0]
all(y == abs(y))
y

###---------------

z <- 0:9
digits <- as.character(z)
digits
d <- as.integer(digits)
all.equal(z, d)

 e <- numeric()
 e[3] <- 17
 e

alpha <- 10*(1:10)
alpha <- alpha[2 * 1:5]
alpha

winter <- data.frame(temp = c(-1,3,2,-2), cat = rep(c("A","B"), 2))
winter
unclass(winter)

###------------ Ordered and unordered factors --------
state <- c("tas", "sa",  "qld", "nsw", "nsw", "nt",  "wa",  "wa",
           "qld", "vic", "nsw", "vic", "qld", "qld", "sa",  "tas",
           "sa",  "nt",  "wa",  "vic", "qld", "nsw", "nsw", "wa",
           "sa",  "act", "nsw", "vic", "vic", "act")
statef <- factor(state)
statef

levels(statef)

incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
             61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)

incmeans <- tapply(incomes, statef, mean)
incmeans

stderr <- function(x) sqrt(var(x)/length(x))

incster <- tapply(incomes, statef, stderr)
incster

##
z <- 1:1500
dim(z) <- c(3,5,100)


x <- array(1:20,dim=c(4,5))   # Generate a 4 by 5 array.
x

i <- array(c(1:3,3:1),dim=c(3,2))
i                             # @code{i} is a 3 by 2 index array.

x[i]                          # Extract those elements

x[i] <- 0                     # Replace those elements by zeros.
x

n <- 60
b <- 5 ; blocks    <- rep(1:b, length= n)
v <- 6 ; varieties <- gl(v,10)

Xb <- matrix(0, n, b)
Xv <- matrix(0, n, v)
ib <- cbind(1:n, blocks)
iv <- cbind(1:n, varieties)
Xb[ib] <- 1
Xv[iv] <- 1
X <- cbind(Xb, Xv)

N <- crossprod(Xb, Xv)
table(blocks,varieties)
all(N == table(blocks,varieties))

h <- 1:17
Z <- array(h, dim=c(3,4,2))
dim(Z) <- c(3,4,2)
Z <- array(0, c(3,4,2))

## So if @code{A}, @code{B} and @code{C} are all similar arrays
## <init>
A <- matrix(1:6, 3,2)
B <- cbind(1, 1:3)
C <- rbind(1, rbind(2, 3:4))
stopifnot(dim(A) == dim(B),
          dim(B) == dim(C))
## <init/>
D <- 2*A*B + C + 1

a <- 1:9
b <- 10*(1:3)

ab <- a %o% b
stopifnot(ab == outer(a,b,"*"),
          ab == outer(a,b))

x <- 1:10
y <- -2:2
f <- function(x, y) cos(y)/(1 + x^2)
z <- outer(x, y, f)


d <- outer(0:9, 0:9)
fr <- table(outer(d, d, "-"))
plot(as.numeric(names(fr)), fr, type="h",
     xlab="Determinant", ylab="Frequency")

##

B <- aperm(A, c(2,1))
stopifnot(identical(B, t(A)))

## for example, @code{A} and @code{B} are square matrices of the same size
## <init>
A <- matrix(1:4, 2,2)
B <- A - 1
## <init/>

A * B

A %*% B


## <init>
x <- c(-1, 2)
## <init/>
x %*% A %*% x

x %*% x
stopifnot(x %*% x == sum(x^2))

xxT <- cbind(x) %*% x
xxT
stopifnot(identical(xxT, x %*% rbind(x)))

## crossprod  ... (ADD)

## diag  ... (ADD)

## linear equations  ... (ADD)

## solve  ... (ADD)

## eigen:
## a symmetric matrix @code{Sm}
## <init>
Sm <- matrix(-2:6, 3); Sm <- (Sm + t(Sm))/4; Sm
## </init>
ev <- eigen(Sm)

evals <- eigen(Sm)$values

##  SVD .....

## "if M is in fact square, then, ..."
## <init>
M <- cbind(1,1:3,c(5,2,3))
X <- cbind(1:9, .25*(-4:4)^2)
X1 <- cbind(1:7, -1)
X2 <- cbind(0,2:8)
y <- c(1:4, 2:6)
## </init>

absdetM <- prod(svd(M)$d)
stopifnot(all.equal(absdetM, abs(det(M))))# since det() nowadays exists

ans <- lsfit(X, y)

 Xplus <- qr(X)
 b <- qr.coef(Xplus, y)
 fit <- qr.fitted(Xplus, y)
 res <- qr.resid(Xplus, y)
##

X <- cbind(1, X1, X2)

vec <- as.vector(X)
vec <- c(X)

statefr <- table(statef)
statefr
statefr <- tapply(statef, statef, length)
statefr

factor(cut(incomes, breaks = 35+10*(0:7))) -> incomef
table(incomef,statef)

###--- @chapter 6. Lists and data frames

Lst <- list(name="Fred", wife="Mary", no.children=3,
            child.ages=c(4,7,9))
Lst$name
Lst$wife
Lst$child.ages[1]
stopifnot(Lst$name == Lst[[1]], Lst[[1]] == "Fred",
          Lst$child.ages[1] == Lst[[4]][1], Lst[[4]][1] == 4
          )

x <- "name" ; Lst[[x]]

## @section  6.2  Constructing and modifying lists

##<init>
Mat <- cbind(1, 2:4)
##</init>
Lst[5] <- list(matrix=Mat)

## @section  6.3  Data frames

accountants <- data.frame(home=statef, loot=incomes, shot=incomef)
## MM: add the next lines to R-intro.texi !
accountants
str(accountants)

## ..........

###--- @chapter 8. Probability distributions

## 2-tailed p-value for t distribution
2*pt(-2.43, df = 13)
## upper 1% point for an F(2, 7)  distribution
qf(0.99, 2, 7)

attach(faithful)
summary(eruptions)

fivenum(eruptions)

stem(eruptions)

hist(eruptions)

## <IMG> postscript("images/hist.eps", ...)
# make the bins smaller, make a plot of density
hist(eruptions, seq(1.6, 5.2, 0.2), prob=TRUE)
lines(density(eruptions, bw=0.1))
rug(eruptions) # show the actual data points
## dev.off() <IMG/>

plot(ecdf(eruptions), do.points=FALSE, verticals=TRUE)

## <IMG> postscript("images/ecdf.eps", ...)
long <- eruptions[eruptions > 3]
plot(ecdf(long), do.points=FALSE, verticals=TRUE)
x <- seq(3, 5.4, 0.01)
lines(x, pnorm(x, mean=mean(long), sd=sqrt(var(long))), lty=3)
## dev.off() <IMG/>

par(pty="s")       # arrange for a square figure region
qqnorm(long); qqline(long)

x <- rt(250, df = 5)
qqnorm(x); qqline(x)

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

shapiro.test(long)

ks.test(long, "pnorm", mean = mean(long), sd = sqrt(var(long)))

##@section One- and two-sample tests

## scan() from stdin :
## can be cut & pasted, but not parsed and hence not source()d
##scn A <- scan()
##scn 79.98 80.04 80.02 80.04 80.03 80.03 80.04 79.97
##scn 80.05 80.03 80.02 80.00 80.02
A <- c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04, 79.97,
       80.05, 80.03, 80.02, 80, 80.02)
##scn B <- scan()
##scn 80.02 79.94 79.98 79.97 79.97 80.03 79.95 79.97
B <- c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97)

## <IMG> postscript("images/ice.eps", ...)
boxplot(A, B)
## dev.off() <IMG/>

t.test(A, B)

var.test(A, B)

t.test(A, B, var.equal=TRUE)

wilcox.test(A, B)

plot(ecdf(A), do.points=FALSE, verticals=TRUE, xlim=range(A, B))
plot(ecdf(B), do.points=FALSE, verticals=TRUE, add=TRUE)

###--- @chapter Grouping, loops and conditional execution


###--- @chapter Writing your own functions


###--- @chapter Statistical models in R


###--- @chapter Graphical procedures

###--- @appendix A sample session

## "Simulate starting a new R session, by
rm(list=ls(all=TRUE))
set.seed(123) # for repeatability

if(interactive())
    help.start()

x <- rnorm(50)
y <- rnorm(x)
plot(x, y)
ls()
rm(x, y)
x <- 1:20
w <- 1 + sqrt(x)/2
dummy <- data.frame(x = x, y = x + rnorm(x)*w)
dummy
fm <- lm(y ~ x, data=dummy)
summary(fm)
fm1 <- lm(y ~ x, data=dummy, weight=1/w^2)
summary(fm1)
attach(dummy)
lrf <- lowess(x, y)
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))
abline(coef(fm1), col = "red")
detach()# dummy

plot(fitted(fm), resid(fm),
     xlab="Fitted values",
     ylab="Residuals",
     main="Residuals vs Fitted")
qqnorm(resid(fm), main="Residuals Rankit Plot")
rm(fm, fm1, lrf, x, dummy)


filepath <- system.file("data", "morley.tab" , package="datasets")
file.show(filepath)
mm <- read.table(filepath)
mm
mm$Expt <- factor(mm$Expt)
mm$Run <- factor(mm$Run)
attach(mm)
plot(Expt, Speed, main="Speed of Light Data", xlab="Experiment No.")
fm <- aov(Speed ~ Run + Expt, data=mm)
summary(fm)
fm0 <- update(fm, . ~ . - Run)
anova(fm0, fm)
detach()
rm(fm, fm0)

x <- seq(-pi, pi, len=50)
y <- x
f <- outer(x, y, function(x, y) cos(y)/(1 + x^2))
oldpar <- par(no.readonly = TRUE)
par(pty="s")
contour(x, y, f)
contour(x, y, f, nlevels=15, add=TRUE)
fa <- (f-t(f))/2
contour(x, y, fa, nlevels=15)
par(oldpar)
image(x, y, f)
image(x, y, fa)
objects(); rm(x, y, f, fa)
th <- seq(-pi, pi, len=100)
z <- exp(1i*th)
par(pty="s")
plot(z, type="l")
w <- rnorm(100) + rnorm(100)*1i
w <- ifelse(Mod(w) > 1, 1/w, w)
plot(w, xlim=c(-1,1), ylim=c(-1,1), pch="+",xlab="x", ylab="y")
lines(z)

w <- sqrt(runif(100))*exp(2*pi*runif(100)*1i)
plot(w, xlim=c(-1,1), ylim=c(-1,1), pch="+", xlab="x", ylab="y")
lines(z)

rm(th, w, z)
## q()

