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
Z <- array(0, c(3,4,2))

if(FALSE)
 D <- 2*A*B + C + 1

a <- 1:9
b <- 10*(1:3)

 ab <- a %o% b
all(ab == outer(a,b,"*"))
outer(a,b)

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

A * B

A %*% B

x %*% A %*% x

ev <- eigen(Sm)

evals <- eigen(Sm)$values

##  SVD .....

absdetM <- prod(svd(M)$d)

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

Lst <- list(name="Fred", wife="Mary", no.children=3,
            child.ages=c(4,7,9))

## ..........

###--- @chapter Probability distributions

## 2-tailed p-value for t distribution
2*pt(-2.43, df = 13)
## upper 1% point for an F(2, 7)  distribution
qf(0.99, 2, 7)

data(faithful)
attach(faithful)
summary(eruptions)

fivenum(eruptions)

stem(eruptions)

hist(eruptions)

# make the bins smaller, make a plot of density
hist(eruptions, seq(1.6, 5.2, 0.2), prob=TRUE)
lines(density(eruptions, bw=0.1))
rug(eruptions) # show the actual data points


library(stepfun)
plot(ecdf(eruptions), do.points=FALSE, verticals=TRUE)

long <- eruptions[eruptions > 3]
plot(ecdf(long), do.points=FALSE, verticals=TRUE)
x <- seq(3, 5.4, 0.01)
lines(x, pnorm(x, mean=mean(long), sd=sqrt(var(long))), lty=3)

par(pty="s")
qqnorm(long); qqline(long)

x <- rt(250, df = 5)
qqnorm(x); qqline(x)

qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

library(ctest)
shapiro.test(long)

