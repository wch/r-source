
## regression test for PR#376
aggregate(ts(1:20), nfreq=1/3)
## Comments: moved from aggregate.Rd


## aperm
# check the names
x <- array(1:24, c(4, 6))
nms <- list(happy=letters[1:4], sad=LETTERS[1:6])

dimnames(x) <- nms
tmp <- aperm(x, c(2, 1))
stopifnot(all.equal(dimnames(tmp), nms[c(2, 1)]))

dimnames(x) <- c(nms[1], list(NULL))
tmp <- aperm(x, c(2, 1))
stopifnot(all.equal(dimnames(tmp), c(list(NULL), nms[1])))

names(nms) <- c("happy", "sad")
dimnames(x) <- nms
tmp <- aperm(x, c(2, 1))
stopifnot(all.equal(names(dimnames(tmp)), names(nms[c(2, 1)])))

dimnames(x) <- c(nms[1], list(NULL))
tmp <- aperm(x, c(2, 1))
stopifnot(all.equal(names(dimnames(tmp)), c("", names(nms)[1])))

# check resize
stopifnot(dim(aperm(x, c(2, 1), FALSE)) == dim(x))
stopifnot(is.null(dimnames(aperm(x, c(2, 1), FALSE))))

# check the types
x <- array(1:24, c(4, 6))
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))
stopifnot(is.integer(aperm(x, c(2, 1))))

x <- x + 0.0
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))
stopifnot(is.double(aperm(x, c(2, 1))))

x <- x + 0.0i
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))

x[] <- LETTERS[1:24]
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))

x <- array(list("fred"), c(4, 6))
x[[3, 4]] <- 1:10
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))
## end of moved from aperm.Rd


## append
stopifnot(append(1:5, 0:1, after=3) == append(1:3, c(0:1, 4:5)))
## end of moved from append.Rd


## array
# funny object, but whatever was the point of that?
utils::str(array(1:3, 0))
## end of moved from array.Rd


## as.POSIXlt
z <- Sys.time()
stopifnot(range(z) == z,
	  min(z) == z,
	  max(z) == z,
	  mean(z) == z)
## end of moved from as.POSIXlt.Rd


## autoload
stopifnot(ls("Autoloads") == ls(envir = .AutoloadEnv))
## end of moved from autoload.Rd


## axis
Y <- c(10.50, 4.0, 13.75, 7.25)
plot(1:4, Y, xlim=c(0,5), ylim=c(0,15))
axis(side=4, at=Y, labels=LETTERS[1:4])
## end of moved from axis.Rd


## backsolve
r <- rbind(c(1,2,3),
	   c(0,1,1),
	   c(0,0,2))
( y <- backsolve(r, x <- c(8,4,2)) ) # -1 3 1
r %*% y # == x = (8,4,2)
( y2 <- backsolve(r, x, transpose = TRUE)) # 8 -12 -5
stopifnot(all.equal(drop(t(r) %*% y2), x))
stopifnot(all.equal(y, backsolve(t(r), x, upper = FALSE, transpose = TRUE)))
stopifnot(all.equal(y2, backsolve(t(r), x, upper = FALSE, transpose = FALSE)))
## end of moved from backsolve.Rd


## basename
dirname(character(0))
## end of moved from basename.Rd


## Bessel
## Check the Scaling :
nus <- c(0:5,10,20)
x <- seq(0,40,len=801)[-1]
for(nu in nus)
   stopifnot(abs(1- besselK(x,nu)*exp( x) / besselK(x,nu,expo=TRUE)) < 2e-15)
for(nu in nus)
   stopifnot(abs(1- besselI(x,nu)*exp(-x) / besselI(x,nu,expo=TRUE)) < 1e-15)
## end of moved from Bessel.Rd


## c
ll <- list(A = 1, c="C")
stopifnot(identical(c(ll, d=1:3), c(ll, as.list(c(d=1:3)))))
## moved from c.Rd


## Cauchy
stopifnot(all.equal(dcauchy(-1:4), 1 / (pi*(1 + (-1:4)^2))))
## end of moved from Cauchy.Rd


## chol
( m <- matrix(c(5,1,1,3),2,2) )
( cm <- chol(m) )
stopifnot(abs(m	 -  t(cm) %*% cm) < 100* .Machine$double.eps)
( Lcm <- La.chol(m) )
stopifnot(abs(m - crossprod(Lcm))  < 100* .Machine$double.eps)

## check with pivoting
( m <- matrix(c(5,1,1,3),2,2) )
( cm <- chol(m, TRUE) )
stopifnot(abs(m	 -  t(cm) %*% cm) < 100* .Machine$double.eps)

x <- matrix(c(1:5, (1:5)^2), 5, 2)
m <- crossprod(x)
Q <- chol(m)
stopifnot(all.equal(t(Q) %*% Q, m))

Q <- chol(m, pivot = TRUE)
pivot <- attr(Q, "pivot")
oo <- order(pivot)
stopifnot(all.equal(t(Q[, oo]) %*% Q[, oo], m))
stopifnot(all.equal(t(Q) %*% Q, m[pivot, pivot]))

# now for something positive semi-definite
x <- cbind(x, x[, 1]+3*x[, 2])
m <- crossprod(x)
qr(m)$rank # is 2, as it should be

(Q <- chol(m, pivot = TRUE)) # NB wrong rank here ... see Warning section.
pivot <- attr(Q, "pivot")
oo <- order(pivot)
stopifnot(all.equal(t(Q[, oo]) %*% Q[, oo], m))
stopifnot(all.equal(t(Q) %*% Q, m[pivot, pivot]))
## end of moved from chol.Rd


## chol2inv
cma <- chol(ma	<- cbind(1, 1:3, c(1,3,7)))
stopifnot(all.equal(diag(3), ma %*% chol2inv(cma)))
stopifnot(all.equal(diag(3), ma %*% La.chol2inv(cma)))
## end of moved from chol2inv.Rd


## col2rgb
pp <- palette(); names(pp) <- pp # add & use names :
stopifnot(col2rgb(1:8) == print(col2rgb(pp)))
stopifnot(col2rgb("#08a0ff") == c(8, 160, 255))
grC <- col2rgb(paste("gray",0:100,sep=""))
stopifnot(grC["red",] == grC["green",],
	  grC["red",] == grC["blue",],
	  grC["red", 1:4] == c(0,3,5,8))
## end of moved from col2rgb.Rd


## colnames
m0 <- matrix(NA, 4, 0)
rownames(m0, do.NULL = FALSE)
colnames(m0, do.NULL = FALSE)
## end of moved from colnames.Rd


## Constants
stopifnot(
 nchar(letters) == 1,
 month.abb == substr(month.name, 1, 3)
)

eps <- .Machine$double.eps
stopifnot(all.equal(pi, 4*atan(1), tol= 2*eps))

# John Machin (1705) computed 100 decimals of pi :
stopifnot(all.equal(pi/4, 4*atan(1/5) - atan(1/239), 4*eps))
## end of moved from Constants.Rd


## cor
stopifnot(  is.na(var(1)),
	  !is.nan(var(1)))

zz <- c(-1.30167, -0.4957, -1.46749, 0.46927)
r <- cor(zz,zz); r - 1
stopifnot(r <= 1) # fails in R <= 1.3.x, for versions of Linux and Solaris
## end of moved from cor.Rd
## Spearman correlations ranked missing values at end <= 1.8.1
X <- cbind(c(1,3,4,NA),c(1,4,2,NA))
X1 <- X[-4,]
stopifnot(all.equal(cor(X,X,method="spearman",use="complete"),
                    cor(X1,X1,method="spearman",use="complete")))
stopifnot(all.equal(cov(X,X,method="spearman",use="complete"),
                    cov(X1,X1,method="spearman",use="complete")))

## DateTimeClasses
(dls <- .leap.seconds[-1] - .leap.seconds[-22])
table(dls)
## end of moved from DateTimeClasses.Rd


## deriv
trig.exp <- expression(sin(cos(x + y^2)))
D.sc <- D(trig.exp, "x")
dxy <- deriv(trig.exp, c("x", "y"))
y <- 1
stopifnot(eval(D.sc) ==
	  attr(eval(dxy),"gradient")[,"x"])
ff <- y ~ sin(cos(x) * y)
stopifnot(all.equal(deriv(ff, c("x","y"), func = TRUE ),
		    deriv(ff, c("x","y"), func = function(x,y){ } )))
## end of moved from deriv.Rd


## diff
x <- cumsum(cumsum(1:10))
stopifnot(diff(x, lag = 2) == x[(1+2):10] - x[1:(10 - 2)],
	  diff(x, lag = 2) == (3:10)^2,
	  diff(diff(x))	   == diff(x, differences = 2))
## end of moved from diff.Rd


## duplicated
x <- c(9:20, 1:5, 3:7, 0:8)
## extract unique elements
(xu <- x[!duplicated(x)])
stopifnot(xu == unique(x), # but unique(x) is more efficient
	  0:20 == sort(x[!duplicated(x)]))

stopifnot(duplicated(iris)[143] == TRUE)
## end of moved from duplicated.Rd


## eigen
Meps <- .Machine$double.eps
set.seed(321, kind = "default")	 # force a particular seed
m <- matrix(round(rnorm(25),3), 5,5)
sm <- m + t(m) #- symmetric matrix
em <- eigen(sm); V <- em$vect
print(lam <- em$values) # ordered DEcreasingly

stopifnot(
 abs(sm %*% V - V %*% diag(lam))	  < 60*Meps,
 abs(sm	      - V %*% diag(lam) %*% t(V)) < 60*Meps)

##------- Symmetric = FALSE:  -- different to above : ---

em <- eigen(sm, symmetric = FALSE); V2 <- em$vect
print(lam2 <- em$values) # ordered decreasingly in ABSolute value !
print(i <- rev(order(lam2)))
stopifnot(abs(lam - lam2[i]) < 60 * Meps)

zapsmall(Diag <- t(V2) %*% V2)
stopifnot( abs(1- diag(Diag)) < 60*Meps)

stopifnot(abs(sm %*% V2 - V2 %*% diag(lam2))		< 60*Meps,
	  abs(sm	 - V2 %*% diag(lam2) %*% t(V2)) < 60*Meps)

## Re-ordered as with symmetric:
sV <- V2[,i]
slam <- lam2[i]
stopifnot(abs(sm %*% sV -  sV %*% diag(slam))		  < 60*Meps)
stopifnot(abs(sm	-  sV %*% diag(slam) %*% t(sV)) < 60*Meps)
## sV  *is* now equal to V  -- up to sign (+-) and rounding errors
stopifnot(abs(c(1 - abs(sV / V)))	<     1000*Meps)
## end of moved from eigen.Rd


## euro
stopifnot(euro == signif(euro,6), euro.cross == outer(1/euro, euro))
## end of moved from euro.Rd


## Exponential
r <- rexp(100)
stopifnot(abs(1 - dexp(1, r) / (r*exp(-r))) < 1e-14)
## end of moved from Exponential.Rd


## family
gf <- Gamma()
stopifnot(1:10 == gf$linkfun(gf$linkinv(1:10)))
## end of moved from family.Rd


## fft
set.seed(123)
eps <- 1e-10  # typically see around 1e-11
for(N in 1:130) {
    x <- rnorm(N)
    if(N %% 5 == 0) {
	m5 <- matrix(x,ncol=5)
	stopifnot(apply(m5,2,fft) == mvfft(m5))
    }
    dd <- Mod(1 - (f2 <- fft(fft(x), inverse=TRUE)/(x*length(x))))
    stopifnot(dd < eps)
}
## end of moved from fft.Rd


## findint
N <- 100
X <- sort(round(rt(N, df=2), 2))
tt <- c(-100, seq(-2,2, len=201), +100)
it <- findInterval(tt, X)

## See that this is N * Fn(.) :
tt <- c(tt,X)
eps <- 100 * .Machine$double.eps
stopifnot(it[c(1,203)] == c(0, 100),
	  all.equal(N * stats::ecdf(X)(tt),
		    findInterval(tt, X),  tol = eps),
	  findInterval(tt,X) ==	 apply( outer(tt, X, ">="), 1, sum)
	  )
## end of moved from findint.Rd
## NA & Inf's :
tt[ina <- c(2,3,5,7)] <- NA
tt[300] <- Inf
X <- c(-Inf, X, Inf)
it <- findInterval(tt,X)
stopifnot(identical(it, as.integer(rowSums(outer(tt, X, ">=")))),
	  is.na(it[ina]))


## fix
oo <- options(editor="touch") # not really changing anything
fix(pi)
if(!is.numeric(pi) || length(pi)!=1 ||
   !is.null(attributes(pi)) || abs(pi - 3.1415) > 1e-4)
      stop("OOPS:  fix() is broken ...")
rm(pi); options(oo)
## end of moved from fix.Rd


## format
(dd <- sapply(1:10, function(i)paste((9:0)[1:i],collapse="")))
np <- nchar(pd <- prettyNum(dd, big.mark="'"))
stopifnot(sapply(0:2, function(m)
	   all(grep("'", substr(pd, 1, np - 4*m)) == (4+3*m):10)))
## end of moved from format.Rd


## Geometric
pp <- sort(c((1:9)/10, 1 - .2^(2:8)))
print(qg <- qgeom(pp, prob = .2))
## test that qgeom is an inverse of pgeom
print(qg1 <- qgeom(pgeom(qg, prob=.2), prob =.2))
stopifnot(identical(qg, qg1))
## moved from Geometric.Rd


## glm
## these are the same -- example from Jim Lindsey
y <- rnorm(20)
y1 <- y[-1]; y2 <- y[-20]
summary(g1 <- glm(y1 - y2 ~ 1))
summary(g2 <- glm(y1 ~ offset(y2)))
Eq <- function(x,y) all.equal(x,y, tol = 1e-12)
stopifnot(Eq(coef(g1), coef(g2)),
	  Eq(deviance(g1), deviance(g2)),
	  Eq(resid(g1), resid(g2)))
## from logLik.glm.Rd
"anorexia" <-
structure(list(Treat = structure(c(2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
), .Label = c("CBT", "Cont", "FT"), class = "factor"), Prewt = c(80.7,
89.4, 91.8, 74, 78.1, 88.3, 87.3, 75.1, 80.6, 78.4, 77.6, 88.7,
81.3, 78.1, 70.5, 77.3, 85.2, 86, 84.1, 79.7, 85.5, 84.4, 79.6,
77.5, 72.3, 89, 80.5, 84.9, 81.5, 82.6, 79.9, 88.7, 94.9, 76.3,
81, 80.5, 85, 89.2, 81.3, 76.5, 70, 80.4, 83.3, 83, 87.7, 84.2,
86.4, 76.5, 80.2, 87.8, 83.3, 79.7, 84.5, 80.8, 87.4, 83.8, 83.3,
86, 82.5, 86.7, 79.6, 76.9, 94.2, 73.4, 80.5, 81.6, 82.1, 77.6,
83.5, 89.9, 86, 87.3), Postwt = c(80.2, 80.1, 86.4, 86.3, 76.1,
78.1, 75.1, 86.7, 73.5, 84.6, 77.4, 79.5, 89.6, 81.4, 81.8, 77.3,
84.2, 75.4, 79.5, 73, 88.3, 84.7, 81.4, 81.2, 88.2, 78.8, 82.2,
85.6, 81.4, 81.9, 76.4, 103.6, 98.4, 93.4, 73.4, 82.1, 96.7,
95.3, 82.4, 72.5, 90.9, 71.3, 85.4, 81.6, 89.1, 83.9, 82.7, 75.7,
82.6, 100.4, 85.2, 83.6, 84.6, 96.2, 86.7, 95.2, 94.3, 91.5,
91.9, 100.3, 76.7, 76.8, 101.6, 94.9, 75.2, 77.8, 95.5, 90.7,
92.5, 93.8, 91.7, 98)), .Names = c("Treat", "Prewt", "Postwt"
), class = "data.frame", row.names = c("1", "2", "3", "4", "5",
"6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16",
"17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
"28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38",
"39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
"50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60",
"61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71",
"72"))
anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
	    family = gaussian, data = anorexia)
summary(anorex.1)
Eq <- function(x,y) all.equal(x,y, tol = 1e-12)
stopifnot(Eq(AIC(anorex.1), anorex.1$aic),
	  Eq(AIC(g1), g1$aic),
	  Eq(AIC(g2), g2$aic))
## next was wrong in 1.4.1
x <- 1:10
lmx <- logLik(lm(x ~ 1)); glmx <- logLik(glm(x ~ 1))
stopifnot(all.equal(as.vector(lmx), as.vector(glmx)),
	  all.equal(attr(lmx, 'df'), attr(glmx, 'df')))
## end of moved from glm.Rd and logLik.glm.Rd


## Hyperbolic
Ceps <- .Machine$double.eps # ``Computer epsilon''
x <- seq(-3, 3, len=200)
stopifnot(
 abs(cosh(x) - (exp(x) + exp(-x))/2) < 20*Ceps,
 abs(sinh(x) - (exp(x) - exp(-x))/2) < 20*Ceps,
 Mod(cosh(x) - cos(1i*x))	< 20*Ceps,
 Mod(sinh(x) - sin(1i*x)/1i)	< 20*Ceps,
 abs(tanh(x)*cosh(x) - sinh(x)) < 20*Ceps
)

stopifnot(abs(asinh(sinh(x)) - x) < 20*Ceps)
stopifnot(abs(acosh(cosh(x)) - abs(x)) < 1000*Ceps) #- imprecise for small x
stopifnot(abs(atanh(tanh(x)) - x) < 100*Ceps)

stopifnot(abs(asinh(x) - log(x + sqrt(x^2 + 1))) < 100*Ceps)
cx <- cosh(x)
stopifnot(abs(acosh(cx) - log(cx + sqrt(cx^2 - 1))) < 1000*Ceps)
## end of moved from Hyperbolic.Rd


## image
## Degenerate, should still work
image(as.matrix(1))
image(matrix(pi,2,4))
x <- seq(0,1,len=100)
image(x, 1, matrix(x), col=heat.colors(10))
image(x, 1, matrix(x), col=heat.colors(10), oldstyle = TRUE)
image(x, 1, matrix(x), col=heat.colors(10), breaks = seq(0.1,1.1,len=11))
## end of moved from image.Rd


## integrate
(ii <- integrate(dnorm, -1.96, 1.96))
(i1 <- integrate(dnorm, -Inf, Inf))
stopifnot(all.equal(0.9500042097, ii$val, tol = ii$abs.err, scale=1),
	  all.equal( 1,		  i1$val, tol = i1$abs.err, scale=1))

integrand <- function(x) {1/((x+1)*sqrt(x))}
(ii <- integrate(integrand, lower = 0, upper = Inf, rel.tol = 1e-10))
stopifnot(all.equal(pi, ii$val, tol = ii$abs.err, scale=1))
## end of moved from integrate.Rd


## is.finite
( weird.values <- c(-20.9/0, 1/0, 0/0, NA) )

Mmax <- .Machine$double.xmax
Mmin <- .Machine$double.xmin
( X.val <- c(Mmin*c(2^(-10:3),1e5,1e10),
	     Mmax*c(1e-10,1e-5,2^(-3:0),1.001)) )
( tst.val <- sort(c(X.val, weird.values), na.last = TRUE) )
( x2 <- c(-1:1/0,pi,1,NA) )
( z2 <- c(x2, 1+1i, Inf -Inf* 1i) )

is.inf <-
  function(x) (is.numeric(x) || is.complex(x)) && !is.na(x) && !is.finite(x)

for(x in list(tst.val, x2, z2))
  print(cbind(format(x), is.infinite=format(is.infinite(x))), quote=FALSE)

rbind(is.nan(tst.val),
      is.na (tst.val))
tst.val [ is.nan(tst.val) !=  is.na(tst.val) ]

stopifnot(
    is.na(0/0),
    !is.na(Inf),
    is.nan(0/0),

    !is.nan(NA)	 &&  !is.infinite(NA)  && !is.finite(NA),
     is.nan(NaN) &&  !is.infinite(NaN) && !is.finite(NaN),
    !is.nan(c(1,NA)),
    c(FALSE,TRUE,FALSE) == is.nan(c   (1,NaN,NA)),
    c(FALSE,TRUE,FALSE) == is.nan(list(1,NaN,NA))#-> FALSE in older versions
)

stopifnot(identical(lgamma(Inf), Inf))
stopifnot(identical(Inf + Inf, Inf))
stopifnot(identical(Inf - Inf, NaN))
stopifnot(identical((1/0) * (1/0), Inf))
stopifnot(identical((1/0) / (1/0), NaN))
stopifnot(identical(exp(-Inf), 0))
stopifnot(identical(log(0), -Inf))
stopifnot(identical((-1)/0, -Inf))
pm <- c(-1,1) # 'pm' = plus/minus
stopifnot(atan(Inf*pm) == pm*pi/2)
## end of moved from is.finite.Rd


## kronecker
( M <- matrix(1:6, ncol=2) )
stopifnot(kronecker(4, M)==4 * M)
# Block diagonal matrix:
stopifnot(kronecker(diag(1, 3), M) == diag(1, 3) %x% M)
## end of moved from kronecker.Rd


## list
str(pl <- as.pairlist(ps.options()))

## These are all TRUE:
stopifnot(is.list(pl) && is.pairlist(pl),
          !is.null(list()),
          is.null(pairlist()),
          !is.list(NULL),
          is.pairlist(pairlist()),
          is.null(as.pairlist(list())),
          is.null(as.pairlist(NULL))
          )
## end of moved from list.Rd


## log
stopifnot(all.equal(log(1:10), log(1:10, exp(1))))
stopifnot(all.equal(log10(30), log(30, 10)))
stopifnot(all.equal(log2(2^pi), 2^log2(pi)))
stopifnot(Mod(pi - log(exp(pi*1i)) / 1i) < 10*.Machine$double.eps)
stopifnot(Mod(1+exp(pi*1i)) < 10*.Machine$double.eps)
## end of moved from Log.Rd


## logistic
eps <- 100 * .Machine$double.eps
x <- c(0:4, rlogis(100))
stopifnot(all.equal(plogis(x),	1 / (1 + exp(-x)), tol = eps))
stopifnot(all.equal(plogis(x, lower=FALSE),  exp(-x)/ (1 + exp(-x)), tol = eps))
stopifnot(all.equal(plogis(x, lower=FALSE, log=TRUE), -log(1 + exp(x)),
		    tol = eps))
stopifnot(all.equal(dlogis(x), exp(x) * (1 + exp(x))^-2, tol = eps))
## end of moved from Logistic.Rd


## Lognormal
x <- rlnorm(1000)	# not yet always :
stopifnot(abs(x	 -  qlnorm(plnorm(x))) < 1e4 * .Machine$double.eps * x)
## end of moved from Lognormal.Rd


## lower.tri
ma <- matrix(1:20, 4, 5)
stopifnot(lower.tri(ma) == !upper.tri(ma, diag=TRUE))
## end of moved from lower.tri.Rd


## make.names
stopifnot(make.names(letters) == letters)
## end of make.names


## mean
x <- c(0:10, 50)
stopifnot(all.equal(mean(x, trim = 0.5), median(x)))
## moved from mean.Rd


## Multinom
N <- 20
pr <- c(1,3,6,10) # normalization not necessary for generation
set.seed(153)
rr <- rmultinom(5000, N, prob = pr)
stopifnot(colSums(rr) == N)
(m <- rowMeans(rr))
all.equal(m, N * pr/sum(pr)) # rel.error ~0.003
stopifnot(max(abs(m/(N*pr/sum(pr)) - 1)) < 0.01)

(Pr <- dmultinom(c(0,0,3), prob = c(1, 1, 14)))
stopifnot(all.equal(Pr, dbinom(3, 3, p = 14/16)))

X <- t(as.matrix(expand.grid(0:3, 0:3)))
X <- X[, colSums(X) <= 3]
X <- rbind(X, 3:3 - colSums(X))
for(p in list(c(1,2,5), 1:3, 3:1, 2:0, 0:2, c(1,2,1), c(0,0,1))) {
  px <- apply(X, 2, function(x) dmultinom(x, prob = p))
  stopifnot(all.equal(sum(px), 1))
}
## end of moved from Multinom.Rd


## plot.lm
# which=4 failed in R 1.0.1
par(mfrow=c(1,1), oma= rep(0,4))
summary(lm.fm2 <- lm(Employed ~ . - Population - GNP.deflator, data = longley))
for(wh in 1:4) plot(lm.fm2, which = wh)
## end of moved from plot.lm.Rd


## Poisson
dpois(c(0, 1, 0.17, 0.77), 1)
## end of moved from Poisson.Rd


## qr
## tests of complex case
set.seed(1)
A <- matrix(rnorm(25), 5, 5, dimnames=list(1:5, letters[1:5]))
qr.solve(A, 1:5)
A[] <- as.complex(A)
qr.coef(qr(A), 1:5)
qr.solve(A, 1:5)

## check for rank-deficient cases
X <- cbind(1:3, 1:3, 1)
stopifnot(all.equal(qr.X(qr(X)), X))
## end of moved from qr.Rd


## qraux
p <- ncol(x <- LifeCycleSavings[,-1]) # not the `sr'
qrstr <- qr(x)	 # dim(x) == c(n,p)
Q <- qr.Q(qrstr) # dim(Q) == dim(x)
R <- qr.R(qrstr) # dim(R) == ncol(x)
X <- qr.X(qrstr) # X == x
stopifnot(all.equal(X,	as.matrix(x)))

## X == Q %*% R :
stopifnot((1 - X /( Q %*% R))< 100*.Machine$double.eps)

dim(Qc <- qr.Q(qrstr, complete=TRUE)) # Square: dim(Qc) == rep(nrow(x),2)
stopifnot((crossprod(Qc) - diag(nrow(x))) < 10*.Machine $double.eps)

QD <- qr.Q(qrstr, D=1:p)      # QD == Q \%*\% diag(1:p)
stopifnot(QD - Q %*% diag(1:p)	< 8* .Machine$double.eps)

dim(Rc <- qr.R(qrstr, complete=TRUE)) # == dim(x)
dim(Xc <- qr.X(qrstr, complete=TRUE)) # square: nrow(x) ^ 2
dimnames(X) <- NULL
stopifnot(all.equal(Xc[,1:p], X))
## end of moved from qraux.Rd


## quantile
x <- rnorm(1001)
n <- length(x) ## the following is exact, because 1/(1001-1) is exact:
stopifnot(sort(x) == quantile(x, probs = ((1:n)-1)/(n-1), names=FALSE))

n <- 777
ox <- sort(x <- round(rnorm(n),1))# round() produces ties
ox <- c(ox, ox[n]) #- such that ox[n+1] := ox[n]
p <- c(0,1,runif(100))
i <- floor(r <- 1 + (n-1)*p)
f <- r - i
stopifnot(abs(quantile(x,p) - ((1-f)*ox[i] + f*ox[i+1])) < 20*.Machine$double.eps)
## end of moved from quantile.Rd


## rep
stopifnot(identical(rep(letters, 0), character(0)),
	  identical(rep.int(1:2, 0), integer(0)))
## end of moved from rep.Rd


## Round
x1 <- seq(-2, 4, by = .5)
non.int <- ceiling(x1) != floor(x1)
stopifnot(
 trunc(x1) == as.integer(x1),
 non.int == (ceiling(x1) != trunc(x1) | trunc(x1) != floor(x1)),
 (signif(x1, 1) != round(x1,1)) == (non.int & abs(x1) > 1)
)
## end of moved from Round.Rd


## seq
stopifnot(
 3 == seq(3,3,	by=pi),
 3 == seq(3,3.1,by=pi),
 seq(1,6,by=3) == c(1,4),
 seq(10,4.05,by=-3) == c(10,7)
)
## end of moved from seq.Rd


## sort
x <- swiss$Education[1:25]
stopifnot(!is.unsorted(sort(x)),
	  !is.unsorted(LETTERS),
	   is.unsorted(c(NA,1:3,2), na.rm = TRUE))

for(n in 1:20) {
    z <- rnorm(n)
    for(x in list(z, round(z,1))) { ## 2nd one has ties
       qxi <- sort(x,  method = "quick",  index.return = TRUE)
       stopifnot(qxi$x == sort(x, method = "shell"),
		 any(duplicated(x)) || qxi$ix == order(x),
		 x[qxi$ix] == qxi$x)
   }
}
## end of moved from sort.Rd


## substr
ss <- substring("abcdef",1:6,1:6)
stopifnot(ss == strsplit ("abcdef",NULL)[[1]])
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
stopifnot(substr(x, 2, 5) == substring(x, 2, 5))
## end of moved from substr.Rd


## svd
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
str(X <- hilbert(9)[,1:6])
str(s <- svd(X))
Eps <- 100 * .Machine$double.eps

D <- diag(s$d)
stopifnot(abs(X - s$u %*% D %*% t(s$v)) < Eps)#	 X = U D V'
stopifnot(abs(D - t(s$u) %*% X %*% s$v) < Eps)#	 D = U' X V

X <- cbind(1, 1:7)
str(s <- svd(X)); D <- diag(s$d)
stopifnot(abs(X - s$u %*% D %*% t(s$v)) < Eps)#	 X = U D V'
stopifnot(abs(D - t(s$u) %*% X %*% s$v) < Eps)#	 D = U' X V
## end of moved from svd.Rd


## Trig
## many of these tested for machine accuracy, which seems a bit extreme
set.seed(123)
stopifnot(cos(0) == 1)
stopifnot(sin(3*pi/2) == cos(pi))
x <- rnorm(99)
stopifnot(all.equal( sin(-x), - sin(x)))
stopifnot(all.equal( cos(-x), cos(x)))
x <- abs(x); y <- abs(rnorm(x))
stopifnot(abs(atan2(y, x) - atan(y/x)) < 10 * .Machine$double.eps)
stopifnot(abs(atan2(y, x) - atan(y/x)) < 10 * .Machine$double.eps)

x <- 1:99/100
stopifnot(Mod(1 - (cos(x) + 1i*sin(x)) / exp(1i*x)) < 10 * .Machine$double.eps)
## error is about 650* are x=0.01
stopifnot(abs(1 - x / acos(cos(x))) < 1000 * .Machine$double.eps)
stopifnot(abs(1 - x / asin(sin(x))) <= 10 * .Machine$double.eps)
stopifnot(abs(1 - x / atan(tan(x))) <= 10 *.Machine$double.eps)
## end of moved from Trig.Rd

## Uniform
u <- runif(20)
stopifnot(punif(u) == u, dunif(u) == 1,
	  runif(100, 2,2) == 2)#-> TRUE [bug in R version <= 0.63.1]
## end of moved from Uniform.Rd


## unique
my.unique <- function(x) x[!duplicated(x)]
for(i in 1:4)
 { x <- rpois(100, pi); stopifnot(unique(x) == my.unique(x)) }

unique(iris)
stopifnot(dim(unique(iris)) == c(149, 5))
## end of moved from unique.Rd


## which.min
stopifnot(length(which.min(numeric(0))) == 0)
stopifnot(length(which.max( c(NA,NA) )) == 0)
## end of moved from which.min.Rd


## Wilcoxon
x <- -1:(4*6 + 1)
fx <- dwilcox(x, 4, 6)
stopifnot(fx == dwilcox(x, 6, 4))
Fx <- pwilcox(x, 4, 6)
stopifnot(abs(Fx - cumsum(fx)) < 10 * .Machine$double.eps)
## end of moved from Wilcoxon.Rd


## .Machine
(Meps <- .Machine$double.eps)
## All the following relations must hold :
stopifnot(
 1 +	 Meps != 1,
 1 + .5* Meps == 1,
 log2(.Machine$double.xmax) == .Machine$double.max.exp,
 log2(.Machine$double.xmin) == .Machine$double.min.exp
)
# This test fails on HP-UX since pow(2,1024) returns DBL_MAX and sets
# errno = ERANGE.  Most other systems return Inf and set errno
if (Sys.info()["sysname"] != "HP-UX")
    stopifnot(is.infinite(.Machine$double.base ^ .Machine$double.max.exp))
## end of moved from zMachine.Rd


## PR 640 (diff.default computes an incorrect starting time)
## By: Laimonis Kavalieris <lkavalieris@maths.otago.ac.nz>
y <- ts(rnorm(24), freq=12)
x <- ts(rnorm(24), freq=12)
arima0(y, xreg = x, seasonal = list(order=c(0,1,0)))
## Comments:


## PR 644 (crash using fisher.test on Windows)
## By: Uwe Ligges <ligges@statistik.uni-dortmund.de>
x <- matrix(c(2, 2, 4, 8, 6, 0, 1, 1, 7, 8, 1, 3, 1, 3, 7, 4, 2, 2, 2,
	      1, 1, 0, 0, 0, 0, 0, 1, 1, 2, 0, 1, 1, 0, 2, 1, 0, 0, 0),
	    nc = 2)
fisher.test(x)
## Comments: (wasn't just on Windows)

## PR 653 (extrapolation in spline)
## By: Ian White <imsw@holyrood.ed.ac.uk>
x <- c(2,5,8,10)
y <- c(1.2266,-1.7606,-0.5051,1.0390)
fn <- splinefun(x, y, method="natural")
xx1 <- fn(0:12)
# should be the same if reflected
fn <- splinefun(rev(-x),rev(y),method="natural")
xx2 <- fn(0:-12)
stopifnot(all.equal(xx1, xx2))
# should be the same as interpSpline
library(splines)
xx3 <- predict(interpSpline(x, y), 0:12)
stopifnot(all.equal(xx1, xx3$y))
unloadNamespace("splines")
## Comments: all three differed in 1.2.1.


## PR 698 (print problem with data frames)
## actually, a subsetting problem with data frames
fred <- data.frame(happy=c(TRUE, FALSE, TRUE), sad=7:9)
z <- try(tmp <- fred[c(FALSE, FALSE, TRUE, TRUE)])
stopifnot(class(z) == "try-error")
## Comments: No error before 1.2.1


## PR 753 (step can't find variables)
##
x <- data.frame(a=rnorm(10), b=rnorm(10), c=rnorm(10))
x0.lm <- lm(a ~ 1, data=x)
step(x0.lm, ~ b + c)
## Comments:


## PR 796 (aic in binomial models is often wrong)
##
a1 <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
	  data = esoph, family = binomial())$aic
a1
a2 <- glm(ncases/(ncases+ncontrols) ~ agegp + tobgp * alcgp,
	  data = esoph, family = binomial(), weights=ncases+ncontrols)$aic
a2
stopifnot(all.equal(a1, a2))
## Comments:
# both should be 236.9645
# changed to use all.equal rather than == in 2.1.0 -pd

## Follow up: example from Lindsey, purportedly of inaccuracy in aic
y <- matrix(c(2, 0, 7, 3, 0, 9), ncol=2)
x <- gl(3, 1)
a <- glm(y ~ x, family=binomial)$aic
stopifnot(is.finite(a))
## Comments: gave NaN prior to 1.2.1


## PR 802 (crash with scan(..., what=list(,,)))
##
m <- matrix(1:9, 3,3)
write(m, "test.dat", 3)
try(scan("test.dat", what=list(,,,)))
unlink("test.dat")
## Comments: segfaulted in 1.2.0


## Jonathan Rougier, 2001-01-30	 [bug in 1.2.1 and earlier]
tmp <- array(list(3), c(2, 3))
tmp[[2, 3]] <- "fred"
all.equal(t(tmp), aperm(tmp))


## PR 860 (Context problem with ... and rbind) Prof Brian D Ripley, 2001-03-03,
f <- function(x, ...)
{
   g <- function(x, ...) x
   rbind(numeric(), g(x, ...))
}
f(1:3)
## Error in 1.2.2
f <- function(x, ...) h(g(x, ...))
g <- function(x, ...) x
h <- function(...)substitute(list(...))
f(1)
## Error in 1.2.2
substitute(list(...))
## Error in 1.2.2


## Martin Maechler, 2001-03-07 [1.2.2 and in parts earlier]
tf <- tempfile()
cat(1:3,"\n", file = tf)
for(line in list(4:6, "", 7:9)) cat(line,"\n", file = tf, append = TRUE)

count.fields(tf) # 3 3 3 : ok {blank line skipped}
z <- scan(tf, what=rep(list(""),3), nmax = 3)
stopifnot(sapply(z, length) == 3)
## FALSE in 1.2.2
z <- as.data.frame(scan(tf, what=rep(list(""),3), n=9))
dim(z)
## should be 3 3.  Was 2 3 in 1.2.2.
read.table(tf)
## gave error in 1.2.2
unlink(tf)


## PR 870 (as.numeric and NAs)	Harald Fekj鎟, 2001-03-08,
is.na(as.numeric(" "))
is.na(as.integer(" "))
is.na(as.complex(" "))
## all false in 1.2.2


## PR 871 (deparsing of attribute names) Harald Fekj鎟, 2001-03-08,
midl <- 4
attr(midl,"Object created") <- date()
deparse(midl)
dump("midl", "midl.R")
source("midl.R") ## syntax error in 1.2.2
unlink("midl.R")


## PR 872 (surprising behavior of match.arg()) Woodrow Setzer, 2001-03-08,
fun1 <- function(x, A=c("power","constant")) {
  arg <- match.arg(A)
  formals()
}
topfun <- function(x, Fun=fun1) {
  a1 <- fun1(x)
  print(a1)
  a2 <- Fun(x,A="power")
  stopifnot(all.equal(a1, a2))
  print(a2)
}
topfun(2, fun1)
## a1 printed without defaults in 1.2.2


## PR 873 (long formulas in terms()) Jerome Asselin, 2001-03-08,
form <- cbind(log(inflowd1),log(inflowd2),log(inflowd3),
    log(inflowd4),log(inflowd5),log(inflowd6)) ~ precip*I(Tmax^2)
terms(form) # error in 1.2.2


## PR 881 Incorrect values in non-central chisq values on Linux, 2001-03-21
x <- dchisq(c(7.1, 7.2, 7.3), df=2, ncp=20)
stopifnot(diff(x) > 0)
## on 1.2.2 on RH6.2 i686 Linux x = 0.01140512 0.00804528 0.01210514


## PR 882 eigen segfaults on 0-diml matrices, 2001-03-23
m <- matrix(1, 0, 0)  # 1 to force numeric not logical
try(eigen(m))
## segfaults on 1.2.2


## 1.3.0 had poor compression on gzfile() with lots of small pieces.
zz <- gzfile("t1.gz", "w")
write(1:1000, zz)
close(zz)
(sz <- file.info("t1.gz")$size)
unlink("t1.gz")
stopifnot(sz < 2000)


## PR 1010: plot.mts (type="p") was broken in 1.3.0 and this call failed.
plot(ts(matrix(runif(10), ncol = 2)), type = "p")


## in 1.3.0 readLines(ok=FALSE) failed.
cat(file="foo", 1:10, sep="\n")
x <- try(readLines("foo", 100, ok=FALSE))
unlink("foo")
stopifnot(length(class(x)) == 1 &&class(x) == "try-error")


## PR 1047 [<-data.frame failure, BDR 2001-08-10
test <- df <- data.frame(x=1:10, y=11:20, row.names=letters[1:10])
test[] <- lapply(df, factor)
test
## error in 1.3.0 in test[]


## PR 1048 bug in dummy.coef.lm, Adrian Baddeley, 2001-08-10
## modified to give a sensible test
old <- getOption("contrasts")
options(contrasts=c("contr.helmert", "contr.poly"))
DF <- data.frame(x=1:20,y=rnorm(20),z=factor(1:20 <= 10))
dummy.coef(lm(y ~ z * I(x), data=DF))
dummy.coef(lm(y ~ z * poly(x,1), data=DF))
## failed in 1.3.0.  Second one warns: deficiency of the method.
options(contrasts=old)


## PR 1050 error in ksmooth C code + patch, Hsiu-Khuern Tang, 2001-08-12
x <- 1:4
y <- 1:4
z <- ksmooth(x, y, x.points=x)
stopifnot(all.equal(z$y, y))
## did some smoothing prior to 1.3.1.


## The length of lines read by scan() was limited before 1.4.0
xx <- paste(rep(0:9, 2000), collapse="")
zz <- file("foo.txt", "w")
writeLines(xx, zz)
close(zz)
xxx <- scan("foo.txt", "", sep="\n")
stopifnot(identical(xx, xxx))
unlink("foo.txt")


## as.character was truncating formulae:  John Fox 2001-08-23
mod <- this ~ is + a + very + long + formula + with + a + very + large + number + of + characters
zz <- as.character(mod)
zz
nchar(zz)
stopifnot(nchar(zz)[3] == 83)
## truncated in 1.3.0


## substr<-, Tom Vogels, 2001-09-07
x <- "abcdef"
substr(x, 2, 3) <- "wx"
stopifnot(x == "awxdef")

x <- "abcdef"
substr(x, 2, 3) <- "wxy"
stopifnot(x == "awxdef")

x <- "abcdef"
substr(x, 2, 3) <- "w"
stopifnot(x == "awcdef")
## last was "aw" in 1.3.1


## reading bytes from a connection,  Friedrich Leisch 2001-09-07
cat("Hello World", file="world.txt")
con <- file("world.txt", "r")
zz <- readChar(con, 100)
close(con)
unlink("world.txt")
stopifnot(zz == "Hello World")
## was "" in 1.3.1.


## prediction was failing for intercept-only model
## as model frame has no columns.
d <- data.frame(x=runif(50), y=rnorm(50))
d.lm <- lm(y ~ 1, data=d)
predict(d.lm, data.frame(x=0.5))
## error in 1.3.1


## predict.arima0 needed a matrix newxreg: Roger Koenker, 2001-09-27
u <- rnorm(120)
s <- 1:120
y <- 0.3*s + 5*filter(u, c(.95,-.1), "recursive", init=rnorm(2))
fit0 <- arima0(y,order=c(2,0,0), xreg=s)
fit1 <- arima0(y,order=c(2,1,0), xreg=s, include.mean=TRUE)
fore0 <- predict(fit0 ,n.ahead=44, newxreg=121:164)
fore1 <- predict(fit1, n.ahead=44, newxreg=121:164)
par(mfrow=c(1,2))
ts.plot(y,fore0$pred, fore0$pred+2*fore0$se, fore0$pred-2*fore0$se,
		gpars=list(lty=c(1,2,3,3)))
abline(fit0$coef[3:4], lty=2)
ts.plot(y, fore1$pred, fore1$pred+2*fore1$se, fore1$pred-2*fore1$se,
		gpars=list(lty=c(1,2,3,3)))
abline(c(0, fit1$coef[3]), lty=2)


## merging when NA is a level
a <- data.frame(x = 1:4)
b <- data.frame(x = 1:3, y = factor(c("NA", "a", "b"), exclude=""))
(m <- merge(a, b, all.x = TRUE))
stopifnot(is.na(m[4, 2]))
## was level NA in 1.3.1
stopifnot(!is.na(m[1, 2]))


## merging with POSIXct columns:
x <- data.frame(a = as.POSIXct(Sys.time() + (1:3)*10000), b = LETTERS[1:3])
y <- data.frame(b = LETTERS[3:4], c = 1:2)
stopifnot(1 == nrow(merge(x, y)))
stopifnot(4 == nrow(merge(x, y, all = TRUE)))


## PR 1149.  promax was returning the wrong rotation matrix.
ability.FA <- factanal(factors = 2, covmat = ability.cov, rotation = "none")
pm <- promax(ability.FA$loadings)
tmp1 <- as.vector(ability.FA$loadings %*% pm$rotmat)
tmp2 <- as.vector(pm$loadings)
stopifnot(all.equal(tmp1, tmp2))
rm(ability.cov)


## PR 1155. On some systems strptime was not setting the month or mday
## when yday was supplied.
bv1 <- data.frame(day=c(346,346,347,347,347), time=c(2340,2350,0,10,20))
attach(bv1)
tmp <- strptime(paste(day, time %/% 100, time %% 100), "%j %H %M")
detach()
stopifnot(tmp$mon == 11)
# day of month will be different in a leap year on systems that default
# to the current year, so test differences:
stopifnot(diff(tmp$mday) == c(0, 1, 0, 0))
## Comments: failed on glibc-based systems in 1.3.1, including Windows.


## PR 1004 (follow up).	 Exact Kolmogorov-Smirnov test gave incorrect
## results due to rounding errors (Charles Geyer, charlie@stat.umn.edu,
## 2001-10-25).
## Example 5.4 in Hollander and Wolfe (Nonparametric Statistical
## Methods, 2nd ed., Wiley, 1999, pp. 180-181).
x <- c(-0.15, 8.6, 5, 3.71, 4.29, 7.74, 2.48, 3.25, -1.15, 8.38)
y <- c(2.55, 12.07, 0.46, 0.35, 2.69, -0.94, 1.73, 0.73, -0.35, -0.37)
stopifnot(round(ks.test(x, y)$p.value, 4) == 0.0524)


## PR 1150.  Wilcoxon rank sum and signed rank tests did not return the
## Hodges-Lehmann estimators of the associated confidence interval
## (Charles Geyer, charlie@stat.umn.edu, 2001-10-25).
## One-sample test: Example 3.1 in Hollander & Wolfe (1973), 29f.
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
we <- wilcox.test(y, x, paired = TRUE, conf.int = TRUE)
## NOTE order: y then x.
## Results from Hollander & Wolfe (1999), 2nd edition, page 40 and 53
stopifnot(round(we$p.value,4) == 0.0391)
stopifnot(round(we$conf.int,3) == c(-0.786, -0.010))
stopifnot(round(we$estimate,3) == -0.46)
## Two-sample test: Example 4.1 in Hollander & Wolfe (1973), 69f.
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
we <- wilcox.test(y, x, conf.int = TRUE)
## NOTE order: y then x.
## Results from Hollander & Wolfe (1999), 2nd edition, page 111 and 126
stopifnot(round(we$p.value,4) == 0.2544)
stopifnot(round(we$conf.int,3) == c(-0.76, 0.15))
stopifnot(round(we$estimate,3) == -0.305)


## range gave wrong length result for R < 1.4.0
stopifnot(length(range(numeric(0))) == 2)
##  Comments: was just NA


## mishandling of integer(0) in R < 1.4.0
x1 <- integer(0) / (1:3)
x2 <- integer(0) ^ (1:3)
stopifnot(length(x1) == 0 & length(x2) == 0)
##  Comments: were integer NAs in real answer in 1.3.1.


## PR#1138/9  rounding could give non-integer answer.
x <- round(100000/3, -2) - 33300
stopifnot(x == 0)
## failed in 1.3.x on Solaris and Windows but not Debian Linux.


## PR#1160 finding midpoints in image <janef@stat.berkeley.edu, 2001-11-06>
x2 <- c(0, 0.002242152, 0.004484305, 0.006726457, 0.00896861,
	0.01121076, 0.01345291, 0.01569507, 0.01793722, 0.02017937,
	0.02242152, 0.02466368, 0.02690583, 0.02914798, 0.03139013,
	0.03363229, 0.03587444, 0.03811659, 0.04035874, 0.04932735,
	0.05156951, 0.05381166)
z <- c(0, 0.067, NA, 0.167, 0.083, 0.05, 0.067, NA, 0, 0.1, 0, 0.05,
       0.067, 0.067, 0.016, 0.117, 0.017, -0.017, 0.2, 0.35, 0.134, 0.15)
image(x2, 1, as.matrix(z))
## Comments: failed under R 1.3.1.


##PR 1175 and 1123##
set.seed(123)
## We can't seem to get Pearson residuals right ##
x <- 1:4 # regressor variable
y <- c(2,6,7,8) # response binomial counts
n <- rep(10,4) # number of binomial trials
ym <- cbind(y,n-y) # response variable as a matrix
glm1 <- glm(ym~x,binomial) # fit a generalized linear model
f <- fitted(glm1)
rp1 <- (y-n*f)/sqrt(n*f*(1-f)) # direct calculation of pearson residuals
rp2 <- residuals(glm1,type="pearson") # should be pearson residuals
stopifnot(all.equal(rp1,rp2))
# sign should be same as response residuals
x <- 1:10
y <- rgamma(10,2)/x
glm2 <- glm(y~x,family=Gamma)
stopifnot(all.equal(sign(resid(glm2,"response")),sign(resid(glm2,"pearson"))))
# shouldn't depend on link for a saturated model
x<-rep(0:1,10)
y<-rep(c(0,1,1,0,1),4)
glm3<-glm(y~x,family=binomial(),control=glm.control(eps=1e-8))
glm4<-glm(y~x,family=binomial("log"),control=glm.control(eps=1e-8))
stopifnot(all.equal(resid(glm3,"pearson"),resid(glm4,"pearson")))


## Torsten Hothorn, 2001-12-04
stopifnot(pt(-Inf, 3, ncp=0) == 0, pt(Inf, 3, ncp=0) == 1)
##  Comments: were 0.5 in 1.3.1


## Paul Gilbert, 2001-12-07
cancor(matrix(rnorm(100),100,1), matrix(rnorm(300),100,3))
##  Comments: failed in R-devel.


## PR#1201: incorrect values in qbeta
x <- seq(0, 0.8, len=1000)
xx <- pbeta(qbeta(x, 0.143891, 0.05), 0.143891, 0.05)
stopifnot(max(abs(x - xx)) < 1e-6)
##  Comments:  Get a range of zeroes in 1.3.1


## PR#1216: binomial null model
y <- rbinom(20, 1, 0.5)
glm(y ~ 0, family = binomial)
##  Comments:  1.3.1 gave  Error in any(n > 1) : Object "n" not found


## Integer overflow in type.convert
res <- type.convert("12345689")
stopifnot(typeof(res) == "integer")
res <- type.convert("12345689012")
stopifnot(typeof(res) == "double")
##  Comments: was integer in 1.4.0


## La.eigen() segfault
#e1 <- La.eigen(m <- matrix(1:9,3))
#stopifnot(e1$values == La.eigen(m, only.values = TRUE)$values)
## 2.0.0: La.eigen is defunct


## Patrick Connelly 2001-01-22, prediction with offsets failed
## a simpler example
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
DF <- data.frame(counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
		 outcome = gl(3, 1, 9), treatment = gl(3, 3),
		 exposure = c(1.17, 1.78, 1.00, 2.36, 2.58, 0.80, 2.51,
		 1.16, 1.77))
fit <- glm(counts ~ outcome + treatment + offset(log(exposure)),
	   family = poisson, data = DF)
p1 <- predict(fit)
p2 <- predict(fit, se = TRUE)  ## failed < 1.4.1
p3 <- predict(fit, newdata = DF)
p4 <- predict(fit, newdata = DF, se = TRUE)
stopifnot(all.equal(p1, p2$fit), all.equal(p1, p3), all.equal(p2, p4))
fit <- glm(counts ~ outcome + treatment, offset = log(exposure),
	   family = poisson, data = DF)
p1 <- predict(fit)
p2 <- predict(fit, se = TRUE)  ## failed < 1.4.1
p3 <- predict(fit, newdata = DF)
p4 <- predict(fit, newdata = DF, se = TRUE)
stopifnot(all.equal(p1, p2$fit), all.equal(p1, p3), all.equal(p2, p4))


## PR#1267 hashing NaN
load(file.path(Sys.getenv("SRCDIR"), "nanbug.rda"))
bb <- b; bb[5] <- NaN
identical(b, bb)	    # TRUE
unique(c(NaN, bb))	    #[1] NaN 0 1 2 3 NA
stopifnot(identical(unique(c(NaN, b)), unique(c(NaN, bb))))
## 1.4.0 gives [1] NaN 0 1 2 NaN 3 NA	on most platforms


## PR 1271  detach("package:base") crashes R.
try(detach("package:base"))


## reported by PD 2002-01-24
Y <- matrix(rnorm(20), , 2)
fit <- manova(Y ~ 1)
fit # failed
print(fit, intercept = TRUE)
summary(fit) # failed
summary(fit, intercept = TRUE)


## Several  qr.*() functions lose (dim)names.
## reported by MM 2002-01-26

## the following should work both in R and S+ :
q4 <- qr(X4 <- cbind(a = 1:9, b = c(1:6,3:1), c = 2:10, d = rep(1,9)))
##q2 <- qr(X4[,1:2])
y04 <- y4 <- cbind(A=1:9,B=2:10,C=3:11,D=4:12)
dimnames(y4)[[1]] <- paste("c",1:9,sep=".")
y1 <- y4[,2]
y40 <- y4 ; dimnames(y40) <- list(dimnames(y4)[[1]], NULL)

c1 <- qr.coef( q4, y4) # row- AND col-names
c2 <- qr.coef( q4, y04)# ditto
c3 <- qr.coef( q4, y40)# row--names
dn3 <- dimnames(c3)
stopifnot(identical(dimnames(c1), dimnames(c2)),
	  identical(dimnames(c1), list(letters[1:4], LETTERS[1:4])),
	  identical(dn3[[1]], letters[1:4]),  length(dn3[[2]]) == 0,
	  identical(names(qr.coef(q4,y1)),   letters[1:4]),
	  identical(dimnames(qr.R(q4))[[2]], letters[1:4]),

	  identical(dimnames(qr.qty(q4,y4)), dimnames(y4)),
	  identical(dimnames(qr.qty(q4,y40)), dimnames(y40)),
	  identical(dimnames(qr.qy (q4,y04)), dimnames(y04)),

	  all.equal(y1,	 qr.fitted(q4, y1 ), tol = 1e-12),
	  all.equal(y4,	 qr.fitted(q4, y4 ), tol = 1e-12),
	  all.equal(y40, qr.fitted(q4, y40), tol = 1e-12),
	  all.equal(y04, qr.fitted(q4, y04), tol = 1e-12),

	  all.equal(X4, qr.X(q4), tol = 1e-12)
)


## PR 1297  read.fwf() was interpreting `#' in 1.4.0/1
cat(file="test.fwf", "123ABC123", "123#3 123", "123XYZ123", sep="\n")
(res <- read.fwf("test.fwf", widths=c(3,3,3), comment.char=""))
unlink("test.fwf")
stopifnot(res[2, 2] == "#3 ")


## abs was failing to dispatch as part of the Math group generic
tmp <- data.frame(x = -5:5)
abs(tmp)
## failed in 1.4.1.


## PR 1363 La.svd was not working for integer args
m <- matrix(1:4, 2)
(s1 <- svd(m))
(s2 <- La.svd(m))
stopifnot(all.equal(s1$d, s2$d), all.equal(s1$u, s2$u),
	  all.equal(s1$v, t(s2$vt)))
(e1 <- eigen(m))
# (e2 <- La.eigen(m)) # 2.0.0: La.eigen is defunct
stopifnot(all.equal(e1$d, e1$d))


## order/sort.list on NA_STRING
x <- c("A", NA, "Z")
stopifnot(identical(sort(x, na.last = TRUE), x[sort.list(x, na.last = TRUE)]))
stopifnot(identical(sort(x, na.last = FALSE), x[sort.list(x, na.last = FALSE)]))
## 1.4.1 sorted NA correctly with sort but not sort.list.


## Don MacQueen 2002-03-26
stopifnot(length(seq(1024902010, 1024902025, by=1)) == 16)
t0 <- ISOdatetime(2002,6,24,0,0,10)
x <- seq.POSIXt(from=t0,to=t0+15,by='1 sec')
stopifnot(length(x) == 16)


## whilst reading the code BDR 2002-03-31
z <- try(max(complex(0)))
stopifnot(inherits(z, "try-error"))
z <- try(min(complex(0)))
stopifnot(inherits(z, "try-error"))
## 1.4.1 gave +-Inf + random imaginary part


## PR#1238  min/max(NULL) or (integer(0))
z <- min(NULL)
stopifnot(!is.na(z), mode(z) == "numeric", z == Inf)
z <- min(integer(0))
stopifnot(!is.na(z), mode(z) == "numeric", z == Inf)
z <- max(NULL)
stopifnot(!is.na(z), mode(z) == "numeric", z == -Inf)
z <- max(integer(0))
stopifnot(!is.na(z), mode(z) == "numeric", z == -Inf)


## more reading the code BDR 2002-03-31
stopifnot(identical(range(), range(numeric(0))))
## in 1.4.1 range() was c(1,1)
stopifnot(is.null(c()))
## in 1.4.1 this was structure(TRUE, names="recursive")

## range(numeric(0)) was not as documented
x <- numeric(0)
(rx <- range(x))
stopifnot(identical(rx, c(min(x), max(x))))
## 1.4.1 had c(NA, NA)


## PR 1431 persp() crashes with numeric values for [x,y,z]lab
persp(1:2, 1:2, matrix(1:4, 2), xlab=1)
## segfaulted in 1.4.1


## PR#1244 bug in det using method="qr"
## method argument is no longer used in det
#m2 <- structure(c(9822616000, 3841723000, 79790.09, 3841723000, 1502536000,
#		  31251.82, 79790.09, 31251.82, 64156419.36), .Dim = c(3, 3))
#(d1 <- det(m2, method="eigenvalues"))
#(d2 <- det(m2, method="qr"))
#stopifnot(d2 == 0) ## 1.4.1 gave 9.331893e+19
#(d3 <- det(m2, method="qr", tol = 1e-10))
#stopifnot(all.equal(d1, d3, tol=1e-3))


## PR#1422 glm start/offset bugs
res <- try(data(ships, package = MASS))
if(!inherits(res, "try-error")) {
ships.glm <- glm(incidents ~ type + year + period + offset(log(service)),
		 family = poisson, data = ships, subset = (service != 0))
update(ships.glm, start = coef(ships.glm))
}
## failed in 1.4.1.


## PR#1439 file.info()$isdir was only partially logical
(info <- file.info("."))
info$isdir
stopifnot(info$isdir == TRUE)
## 1.4.1 had a TRUE value that was not internally integer 1.

## PR#1473 predict.*bSpline() bugs extrapolating for deriv >= 1
library(splines)
x <- c(1:3,5:6)
y <- c(3:1,5:6)
(isP <- interpSpline(x,y))# poly-spline representation
(isB <- interpSpline(x,y, bSpl = TRUE))# B-spline repr.
xo <- c(0, x, 10)# x + outside points
op <- options(digits = 4)
for(der in 0:3) # deriv=3 fails!
    print(formatC(try(predict(isP, xo, deriv = der)$y), wid=7,format="f"),
	  quote = FALSE)
## and for B-spline (instead of polynomial):
for(der in 0:3)	 # deriv=3 failed
    print(formatC(try(predict(isB, xo, deriv = der)$y), wid=7,format="f"),
	  quote = FALSE)
options(op)
unloadNamespace("splines")


## PR 902 segfaults when warning string is too long, Ben Bolker 2001-04-09
provoke.bug <- function(n=9000) {
   warnmsg <- paste(LETTERS[sample(1:26,n,replace=TRUE)],collapse="")
   warning(warnmsg)
}
provoke.bug()
## segfaulted in 1.2.2, will also on machines without vsnprintf (none now)


## PR#1510 merge with multiple match rows and different names.
df1 <- data.frame(z = 1:10, m = letters[1:10], w = rnorm(10))
df2 <- data.frame(x = 1:10, y = rnorm(10), n = letters[1:10])
merge(df2, df1, by.x = c("x", "n"), by.y = c("z", "m"))
## failed in 1.5.0


## PR 1524  Problems with paste/unlist
l <- names(unlist(list(aa = list(bb = 1))))
l
# this is exactly "aa.bb"
stopifnot(identical(l, "aa.bb"))
l2 <- paste(l, "this should be added")
stopifnot(identical(l2, "aa.bb this should be added"))
## 1.5.0 gave l2 printing as l.


## PR 1530 drop inconsistency for data frames
DF <- data.frame(x = 1:3, y = c("A","D","E"), z = c(6,9,10))
a1 <- DF[1,1:3]
xx <- DF[1,]
a2 <- xx[, 1:3]
a3 <- DF[1,1:3, drop = TRUE]
a4 <- xx[, 1:3, drop = TRUE]
stopifnot(identical(a1, a2), identical(a3, a4))
## <= 1.5.0 had a2 == a3.


## PR 1536 rbind.data.frame converts logical to factor
df <- data.frame(a = 1:10)
df$b <- df$a < 5
ddf <- rbind(df, df)
stopifnot(!is.factor(ddf$b))
## 1.5.0 had b as a factor.


## PR 1548 : prettyNum inserted leading commas
stopifnot(prettyNum(123456, big.mark=",") == "123,456")


## PR 1552: cut.dendrogram
hc <- hclust(dist(USArrests), "ave")
cc <- cut(as.dendrogram(hc), h = 20)## error in 1.5.0

## predict.smooth.spline(*, deriv > 0) :
x <- (1:200)/32
ss <- smooth.spline(x, 10*sin(x))
stopifnot(length(x) == length(predict(ss,deriv=1)$x))# not yet in 1.5.0

## pweibull(large, log=T):
stopifnot(pweibull(seq(1,50,len=1001), 2,3, log = TRUE) < 0)

## selfStart.default() w/ no parameters:
## --> make this into example(selfStart) {with data and nls()!}
logist <- deriv( ~Asym/(1+exp(-(x-xmid)/scal)), c("Asym", "xmid", "scal"),
		function(x, Asym, xmid, scal){} )
logistInit <- function(mCall, LHS, data) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    if(nrow(xy) < 3) stop("Too few distinct input values to fit a logistic")
    Asym <- max(abs(xy[,"y"]))
    if (Asym != max(xy[,"y"])) Asym <- -Asym  # negative asymptote
    xmid <- NLSstClosestX(xy, 0.5 * Asym)
    scal <- NLSstClosestX(xy, 0.75 * Asym) - xmid
    value <- c(Asym, xmid, scal)
    names(value) <- mCall[c("Asym", "xmid", "scal")]
    value
}
logist <- selfStart( logist, initial = logistInit ) ##-> Error in R 1.5.0
str(logist)


## part of PR 1662: fisher.test with total one
fisher.test(cbind(0, c(0,0,0,1)))
## crashed in R <= 1.5.0

stopifnot(Mod(vector("complex", 7)) == 0) # contained garbage in 1.5.0

## hist.POSIXt with numeric `breaks'
hist(.leap.seconds, breaks = 5)
## error in 1.5.1


##Jonathan Rougier 2002-06-18
x <- matrix(runif(30), 10, 3)
poly(x, degree=2)
## failed in 1.5.1


## PR#1694 cut with infinite values -> NA (Markus J鋘tti)
cut.off <- c(-Inf, 0, Inf)
x <- c(-Inf, -10, 0, 10, Inf)
(res <- cut(x, cut.off, include.lowest=TRUE))
stopifnot(!is.na(res))
(res <- cut(x, cut.off, include.lowest=TRUE, right=FALSE))
stopifnot(!is.na(res))
## outer values were NA in 1.5.1


## ls.str() for function environments:
Fn <- ecdf(rnorm(50))
ls.str(envir = environment(Fn))
## failed in 1.5.1


## PR 1767 all.equal.character for non-matching NAs
all.equal(c("A", "B"), c("A", NA))
## failed in 1.5.1


## failed since at least version 0.90:
stopifnot(is.character(a12 <- all.equal(1,1:2)),
	  length(a12) == 1,# was 2 till 1.6.2
	  a12 == "Numeric: lengths (1, 2) differ")
## a12 was *list* of length 3


## related to PR 1577/1608, conversions to character
DF <- data.frame(b = LETTERS[1:3])
sapply(DF, class)
DF[[1]] <- LETTERS[1:3]
stopifnot(is.character(DF$b)) ## was factor < 1.6.0
DF <- data.frame(b = LETTERS[1:3])
DF$b <- LETTERS[1:3]
stopifnot(is.character(DF$b)) ## always was character.

x <- data.frame(var = LETTERS[1:3]); x$var <- as.character(x$var)
x[[1]][2] <- "3"
x
stopifnot(is.character(x$var))
is.na(x[[1]]) <- 2
stopifnot(is.character(x$var))

x <- data.frame(var = I(LETTERS[1:3]))
x[[1]][2] <- "3"
x
stopifnot(is.character(x$var))
is.na(x[[1]]) <- 2
stopifnot(is.character(x$var))

x <- data.frame(var = LETTERS[1:3])
x[[1]][2] <- "3"
x
stopifnot(is.factor(x$var))
is.na(x[[1]]) <- 2
stopifnot(is.factor(x$var))

x <- data.frame(a = 1:4)
y <- data.frame(b = LETTERS[1:3])
y$b <- as.character(y$b)
z <- merge(x, y, by = 0, all.x = TRUE)
sapply(z, data.class)
stopifnot(is.character(z$b))
## end of `related to PR 1577/1608'


## logicals became factors < 1.6.0
stopifnot(sapply(as.data.frame(matrix((1:12)%% 4 == 1, 3,4)),
		 is.logical))


## recycling of factors in data.frame (wish from PR#1713)
data.frame(x=c("A","B"), y="C")	     # failed to recycle in 1.5.1
X <- data.frame(x=c("A","B"), y=I("C")) # also failed
XX <- data.frame(x=c("A","B"), y=I(rep("C", 2))) # fine
stopifnot(identical(X, XX))
## Last is false in some S variants.


## test of rank-deficient prediction, as various claims this did not work
## on R-help in June 2002
x1 <- rnorm(100)
x3 <- rnorm(100)
y <- rnorm(100)
train <- data.frame(y=y, x1=x1, x2=x1, x3=x3)
fit <- lm(y ~ ., train)
stopifnot(all.equal(predict(fit), predict(fit, train)))
## warning added for 1.6.0


## terms(y ~ .) on data frames with duplicate names
DF <- data.frame(y = rnorm(10), x1 = rnorm(10), x2 = rnorm(10), x3 = rnorm(10))
names(DF)[3] <- "x1"
fit <- try(lm(y ~ ., DF))
stopifnot(class(fit) == "try-error")
## had formula y ~ x1 + x1 + x3 in 1.5.1.


## PR#1759 as.character.octmode() (Henrik Bengtsson)
x <- 0; class(x) <- "octmode"
stopifnot(as.character(x) == "0")
## gave "" in 1.5.1


## PR#1843 unsplit() with f a list
g <- factor(round(10 * runif(1000)))
x <- rnorm(1000) + sqrt(as.numeric(g))
xg <- split(x, list(g1=g,g2=g))
res <- unsplit(xg, list(g1=g, g2=g))
stopifnot(x == res) # can't have rounding error here
## gave incorrect result with warning in 1.5.1.


## matching NAs on Solaris (MM 2002-08-02)
# x <- as.double(NA)
# identical(x + 0, x)
# stopifnot(match(x + 0, x, 0) == 1)
## match failed on Solaris with some compiler settings
## NA+0 is not guaranteed to be NA: could be NaN


## identical on specials  (BDR 2002-08-02)
stopifnot(identical(as.double(NA), NaN) == FALSE)
## was identical on 1.5.1


## safe prediction (PR#1840)
cars.1 <- lm(dist ~ poly(speed, degree = 1), data = cars)
cars1  <- lm(dist ~	 speed,		     data = cars)
DF <- data.frame(speed=4)
stopifnot(all.equal(predict(cars.1, DF), predict(cars1, DF)))
## error in 1.5.1


## Ops.data.frame (PR#1889)
d <- data.frame(1:10)
d > list(5)
## failed in 1.5.1


## order(na.last = NA) (PR#1913 / 1906 / 1981)
x <- 1
order(x, na.last=NA)
order(x, x, x, na.last=NA)
## failed in 1.5.1, since sapply simplified to a scalar.
stopifnot(3:1 == order(c(1,2,3,NA), na.last=NA, decreasing=TRUE))
## ignored `decreasing' in 1.5.1
order(c(NA, NA), na.last = NA)
## error in 1.5.1, now integer(0)

## as.list() coerced logical to integer (PR#1926)
x <- c(TRUE,FALSE,NA)
stopifnot(identical(x, unlist(as.list(x))))
## the 2nd was (1,0,NA) before 1.6


## test of long Error expression in aov(): PR#1315 and later,
## and also a cross-check of deparse(, cutoff = 500)
AA <- structure(list(Y2 = c(10, 9, 0, 0, 5, 6, 0, 0, 8, 9, 0, 0, 4,
4, 0, 0, 12, 11, 2, 0, 6, 7, 0, 0), P2 = structure(c(1, 1, 1,
1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3
), .Label = c("1", "2", "3"), class = "factor"), AAAAAAAA = structure(c(1,
1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2,
2, 2), .Label = c("E1", "E2"), class = "factor"), B2 = structure(c(1,
1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1,
2, 2), .Label = c("Red", "Unred"), class = "factor"), C2 = structure(c(1,
2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
1, 2), .Label = c("Agent", "Patient"), class = "factor")), .Names = c("Y2",
"P2", "AAAAAAAA", "B2", "C2"), class = "data.frame", row.names = c("1",
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
"14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"
))
AK2anova.out <-
    aov(Y2 ~ AAAAAAAA * B2 * C2 +
	Error(P2 + P2:AAAAAAAA + P2:B2 + P2:C2 + P2:AAAAAAAA:B2 +
	      P2:AAAAAAAA:C2 + P2:B2:C2 + P2:AAAAAAAA:B2:C2),
	data=AA)
## failed in 1.5.1

## as.character was silently truncating expressions to 60 chars
q2 <- expression(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
(q3 <- as.character(q2))
stopifnot(nchar(q3) == 68)
## was 61 in 1.5.1


## Ops wasn't using NextMethod correctly

## Ops.ordered:
or <- ordered(c("a","b","c"))
stopifnot( (or == "a") == c(TRUE,FALSE,FALSE))
stopifnot(or == or)
stopifnot(or != "d")
##  last was NA NA NA in 1.5.1


Ops.foo <- function(e1, e2) {
    NextMethod()
}
Ops.baz <- function(e1, e2) {
   NextMethod()
}
a <- b <- 1
class(a) <- c("foo","bar","baz")
class(b) <- c("foo","baz")
stopifnot(a == 1,
	  b == a)
##(already worked in 1.5.1)


## t() wrongly kept "ts" class and "tsp"
t(ts(c(a=1, d=2)))
## gave error while printing in 1.5.1
at <- attributes(t(ts(cbind(1, 1:20))))
stopifnot(length(at) == 2,
	  at$dim == c(2, 20),
	  at$dimnames[[1]] == paste("Series", 1:2))
## failed in 1.5.1


## Nextmethod from anonymous function (PR#1211)
try( get("print.ts")(1) )# -> Error
## seg.faulted till 1.5.1


## cbind/rbind should work with NULL only args
stopifnot(is.null(cbind(NULL)), is.null(cbind(NULL,NULL)),
	  is.null(rbind(NULL)), is.null(rbind(NULL,NULL)))
## gave error from 0.63 till 1.5.1


## seq.POSIXt() had rounding problem:
stopifnot(4 == length(seq(from=ISOdate(2000,1,1), to=ISOdate(2000,1,4),
			  length.out=4)))
## length was 5 till 1.6.0


## loess has a limit of 4 predictors (John Deke on R-help, 2002-09-16)
data1 <- array(runif(500*5),c(500,5))
colnames(data1) <- c("x1","x2","x3","x4","x5")
y <- 3+2*data1[,"x1"]+15*data1[,"x2"]+13*data1[,"x3"]-8*data1[,"x4"]+14*data1[,"x5"]+rnorm(500)
data2 <- as.data.frame(cbind(y,data1))
result4 <- loess(y~x1+x2+x3+x4,data2)
try(result5 <- loess(y~x1+x2+x3+x4+x5,data2))
## segfaulted in 1.5.1


## format.AsIs was not handling matrices
jk <- data.frame(x1=2, x2=I(matrix(0,1,2)))
jk
## printing failed in 1.5.1


## eigenvectors got irrelevant names (PR#2116)
set.seed(1)
A <- matrix(rnorm(20), 5, 5)
dimnames(A) <- list(LETTERS[1:5], letters[1:5])
(ev <- eigen(A)$vectors)
stopifnot(is.null(colnames(ev)))
## had colnames in 1.6.0


## pretty was not pretty {because seq() isn't} (PR#1032 and D.Brahm)
stopifnot(pretty(c(-.1, 1))[2] == 0, ## [2] was -2.775558e-17
	  pretty(c(-.4,.8))[3] == 0, ## [3] was 5.551115e-17
	  pretty(100+ c(0, pi*1e-10))[4] > 100,# < not too much rounding!
	  pretty(c(2.8,3))[1] == 2.8)
## last differed by 4.44e-16 in R 1.1.1


## add1 was giving misleading message when scope was nonsensical.
counts <- c(18,17,15,20,10,20,25,13,12)
fit <- glm(counts ~ 1, family=poisson)
res <- try(add1(fit, ~ .))
## error in 1.6.0 was
## `Error in if (ncol(add) > 1) { : missing value where logical needed'
stopifnot(length(grep("missing value", res)) == 0)


## stripchart with NAs (PR#2018)
Sepal <- iris$Sepal.Length
Sepal[27] <- NA
stripchart(Sepal ~ iris$Species, method="stack")
## failed in 1.6.1


## losing is.object bit internally (PR#2315)
stopifnot(is.ts(log(as.ts(1:10))))
## failed for integer original as here in 1.6.1.


## formatC ignored rounding up (PR#2299)
stopifnot(formatC(99.9, 1, format="fg") == "100")
stopifnot(formatC(99.9, 2, format="fg") == "100")
stopifnot(formatC(99.9, 3, format="fg") == "99.9")
## gave exponential format on 1.6.1


## full/partial matching in attr.
tmp <- list(id=1)
attr(tmp,"n.ch") <- 2
attr(tmp,"n") <- 1
attributes(tmp)
(res <- attr(tmp, "n"))
stopifnot(length(res) == 1 && res == 1)
## gave NULL in 1.6.1


## Undocumented line limit in system(intern=TRUE)
## Naoki Takebayashi <ntakebay@bio.indiana.edu> 2002-12-07
tmp <- tempfile()
long <- paste(rep("0123456789", 20), collapse="")
cat(long, "\n", sep="", file=tmp)
junk <- system(paste("cat", tmp), intern = TRUE)
stopifnot(length(junk) == 1, nchar(junk[1]) == 200)
## and split truncated on 1.6.1


## missing group generics for `difftime' (related to PR#2345)
x <- as.difftime(c("0:3:20", "11:23:15"))
y <- ISOdate(2001, 4, 26) - ISOdate(2001, 2, 26)
x + x
2*x
x < y
x < 100
## all but last failed in R < 1.7.0


## PR 2358 (part)
mm <- 1:2
names(mm)[2] <- 'y'
(mm <- c(mm, 3))
stopifnot(is.na(names(mm)[1]))
## 1.6.1 had "NA"


## PR 2357
a <- matrix(c(1,2,3,-1,-2,3),2,3,dimnames=list(c("A","B"),NULL))
(z <- pmax(a, 0))
stopifnot(identical(dimnames(z), dimnames(a)))
# further checks
a <- matrix(c(1,2,3,-1,-2,3),2,3,dimnames=list(c("A","B"),1:3))
(z <- pmax(a, 0))
stopifnot(identical(dimnames(z), dimnames(a)))
a <- matrix(c(1,2,3,-1,-2,3),2,3,dimnames=list(NULL, letters[1:3]))
(z <- pmax(a, 0))
stopifnot(identical(dimnames(z), dimnames(a)))
## 1.6.1 only transferred dimnames if both components were non-null


## internal conversion to factor in type.convert was not right
## if a character string NA was involved.
x <- c(NA, "NA", "foo")
(z <- type.convert(x))
stopifnot(identical(levels(z), "foo"))
(z <- type.convert(x, na.strings=character(0)))
stopifnot(identical(levels(z), sort(c("foo", "NA"))))
(z <- type.convert(x, na.strings="foo"))
stopifnot(identical(levels(z), "NA"))
## extra level in 1.6.1


## related example
tmp <- tempfile()
cat(c("1", "foo", "\n", "2", "NA", "\n"), file = tmp)
(z <- read.table(tmp, na.strings="foo"))
unlink(tmp)
stopifnot(identical(levels(z$V2), "NA"),
	  identical(is.na(z$V2), c(TRUE, FALSE)))
## 1.6.1 had V2 as NA NA.


## PR#2396, parsing and pushbacks.
tmp <- tempfile()
cat( c( "1", "a+b", "2"), file=tmp, sep="\n")
open(tcon <- file(tmp))
readLines(tcon, n=1)
pushBack("a1+b1", tcon)
parse(file=tcon, n=1)
close(tcon)
unlink(tmp)
## failed with syntax error in 1.6.1


## NAs in max.col
a <- matrix(1, 3, 3)
a[1,2] <- NA
(z <- max.col(a))
stopifnot(is.na(z[1]))
## gave (randomly) 1 or 3 in 1.6.1


## PR#2469: read.table on MacOS CR-terminated files.
tmp <- tempfile()
x <- c("aaa", "bbb", "ccc")
cat(x, sep="\r", file=tmp)
con <- file(tmp)
open(con)
line <- readLines(con, 1)
pushBack(line, con)
(y <- readLines(con))
close(con)
unlink(tmp)
stopifnot(identical(x, y))
## pushback problems in 1.6.2 only


## dimnames in solve(): not a bug just an improvement in 1.7.0
A <- diag(3)
dimnames(A) <- list(LETTERS[1:3], letters[1:3])
(B <- solve(A))
stopifnot(identical(colnames(B), rownames(A)))
## R < 1.7.0 had no colnames for B, and S has the colnames of A.
stopifnot(all.equal(t(B), solve(t(A))))
## test here is of dimnames


## PR#2507: extracting 0-length dimensions for arrays
dn <- list(LETTERS[1:2], letters[1:3], paste("t",1:4,sep=""))
A. <- array(1:24, dim = 2:4, dimnames = dn)
str(A.[1, 0, 2 ])
str(A.[1, 0, 2, drop = FALSE])
## both gave errors in 1.6.2

plot(sf <- stepfun(2, 3:4))
## failed in 1.6.2


## PR#2541, cbind (and rbind) with zero-length components
y <- matrix(0,1,0)
cbind(y, integer(0))
y <- matrix(0,0,1)
rbind(y, integer(0))
## gave fatal error in 1.6.2, since miscalculated no of rows/cols.


## PR#2518 multiple objects in AIC.default.
lm1 <- lm(y ~ x, list(x=1:10, y=jitter(1:10)))
lm2 <- lm(y ~ x, list(x=1:10, y=jitter(1:10)))
AIC(lm1, lm2)
AIC(lm1, lm2,  k=2)
## second failed in 1.6.2


## PR#2591 unique on ordered factor
f <- ordered(month.name, levels=month.name)
(uf <- unique(f))
stopifnot(is.ordered(uf))
## gave factor in 1.6.2


## PR#2587 coercion of length-0 vectors
x <- numeric(0)
x[1] <- NA
stopifnot(identical(mode(x), "numeric"))
##


## PR#2586 labelling in alias()
Y <- c(0,1,2)
X1 <- c(0,1,0)
X2 <- c(0,1,0)
X3 <- c(0,0,1)
(res <- alias(lm(Y ~ X1 + X2 + X3)))
stopifnot(identical(rownames(res[[2]]),	 "X2"))
## the error was in lm.(w)fit


## coercion lost the object bit in [<-
x <- I(TRUE)
is.object(x)
x[2] <- "N"
stopifnot(is.object(x))
## failed in 1.6.2


## check inherits now works for basic classes:
x <- 1:3
is.object(x) # FALSE
stopifnot(inherits(x, "integer"))
## 2003-Mar-12 it did not


## rank() is numeric also for NA char vectors
stopifnot(is.numeric(rk <- rank(c("ch","c", NA))),
          all(rk == c(2,1,3)))
## did not from R 1.2 -- 1.6


## table() should by default keep NA levels of factors
i <- c(1:2,NA);	 fi <- factor(i, exclude = NULL)
stopifnot(identical(as.character(i), dimnames(table(fi))[[1]]))
## not in 2003-Mar-10 unstable


## [lm.]influence() for multivariate lm :
n <- 32
Y <- matrix(rnorm(3 * n), n, 3)
X <- matrix(rnorm(5 * n), n, 5)
infm <- lm.influence(mod <- lm(Y ~ X))
## failed up to 2003-03-29 (pre 1.7.0)


## rbind.data.frame with character and ordered columns
A <- data.frame(a=1)
A$b <- "A"
B <- data.frame(a=2)
B$b <- "B"
AB <- rbind(A,B)
(cl <- sapply(AB, class))
stopifnot(cl[2] == "character") # was factor in 1.6.2

A <- data.frame(a=1:3, b=ordered(letters[1:3]))
B <- data.frame(a=7:9, b=ordered(letters[7:9]))
AB <- rbind(A,B)
(cl <- sapply(AB, class))
stopifnot(cl$b[1] == "ordered") # was factor in 1.6.2
C <- data.frame(a=4:6, b=letters[4:6])
ABC <- rbind(AB, C)
(cl <- sapply(ABC, class))
stopifnot(cl[2] == "factor")

A <- data.frame(a=1)
A$b <- "A"
B <- data.frame(a=2, b="B")
(AB <- rbind(A,B))
(cl <- sapply(AB, class))
stopifnot(cl[2] == "character")

A <- data.frame(a=1, b="A")
B <- data.frame(a=2)
B$b <- "B"
(AB <- rbind(A,B))
(cl <- sapply(AB, class))
stopifnot(cl[2] == "factor")
A <- data.frame(a=c("A", NA, "C"))
B <- data.frame(a=c("B", NA, "C"))
(AB <- rbind(A,B))
stopifnot(levels(AB$a) == c("A", "C", "B"))
A <- data.frame(a=I(c("A", NA, "C")))
B <- data.frame(a=I(c("B", NA, "C")))
(AB <- rbind(A,B))
(cl <- sapply(AB, class))
stopifnot(cl[1] == "AsIs")

A <- data.frame(a=1)
A$b <- "A"
B <- data.frame(a=2, b=I("B"))
(AB <- rbind(A,B))
(cl <- sapply(AB, class))
stopifnot(cl[2] == "character")

A <- data.frame(a=1, b="A")
B <- data.frame(a=2, b=I("B"))
(AB <- rbind(A,B))
(cl <- sapply(AB, class))
stopifnot(cl[2] == "factor")
##


## hclust(), as.hclust.twins(), agnes()  consistency
x <- matrix(rnorm(30), ncol=3)  # no observation names
xn <- x; rownames(xn) <- letters[10:1]# has obs. names
hc  <- hclust(dist(x),  method="complete")
hcn <- hclust(dist(xn), method="complete")
iC1 <- !names(hc) %in% c("labels", "call")
stopifnot(identical(hc, hhc <- as.hclust(hc)),
          identical(hhc, as.hclust(hhc)),
          identical(hc[iC1], hcn[iC1]),
          identical(hcn$labels, rownames(xn))
          )

if(require(cluster)) { # required package
  ag <- agnes(x, method="complete")
  hcag <- as.hclust(ag)
  agn <- agnes(xn, method="complete")
  hcagn <- as.hclust(agn)
  iC2 <- !names(hcag) %in% c("labels", "call")
  stopifnot(identical(hcagn[iC2], hcag[iC2]),
            identical(hcagn$labels, hcn$labels),
            all.equal(hc$height, hcag$height, tol = 1e-12),
            all(hc$merge == hcag$merge | hc$merge == hcag$merge[ ,2:1])
            )
  detach("package:cluster")
}
## as.hclust.twins() lost labels and more till (incl) 1.6.2


## PR#2867 qr(LAPACK=TRUE) didn't always pivot in 1.7.0
set.seed(1)
X <- matrix(rnorm(40),10,4)
X[,1] <- X[,2]
(qrx <- qr(X, LAPACK=TRUE))
stopifnot(any(qrx$pivot != 1:4)) # check for pivoting
##


## rownames<- did not work on an array with > 2 dims in 1.7.0
A <- array(1:12, dim=c(2, 3, 2))
rownames(A) <- letters[1:2]
A <- array(1:12, dim=c(2, 3, 2))
colnames(A) <- 1:3
## failed in 1.7.0


## predict on constant model, PR#2958
res <- model.frame(~1, data.frame(x = 1:5))
stopifnot(nrow(res) == 5)
res <- predict(lm(y ~ 1, data = data.frame(y = rep(0:3, c(5,9,7,1)))),
               newdata = data.frame(x = 1:5))
stopifnot(length(res) == 5)
res <- predict(glm(y ~ 1, family = poisson,
                   data = data.frame(y = rep(0:3, c(5,9,7,1)))),
               newdata = data.frame(x = 1:5), type = "r")
stopifnot(length(res) == 5)
## all length one in 1.7.0


## PR#3035 problems with sep > ASCII(127)
f <- tempfile()
cat("x", "1񕺷3�19�25", "2񖎣4�20�26", "3񖢏5�21�27",
    "4�10�16�22�28", "5�11�17�23�29", "6�12�18�24�30", sep="\n", file=f)
read.table(f, header = TRUE, sep ="�")
## failed in 1.7.0


## PR#2993 need to consider delta=NULL in power.t.test{ctest}
power.t.test(n=10, delta=NULL, power=.9, alternative="two.sided")
## failed in 1.7.0


## PR#3221 eigenvectors should be a matrix even in the 1x1 case
A <- matrix(1)
stopifnot(is.matrix(eigen(A)$vectors))
stopifnot(is.matrix(eigen(A, EISPACK = TRUE)$vectors))
# stopifnot(is.matrix(La.eigen(A)$vectors)) defunct in 2.0.0
## gave vector in 1.7.0


## [[<-.data.frame
testdata <- data.frame(a=1:2, b = rep(NA, 2))
try(testdata[["a"]] <- strptime(c("31121991", "31121991"), "%d%m%Y"))
stopifnot(inherits(.Last.value, "try-error"))
## succeeded in 1.7.0


## pacf on n x 1 matrix: Paul Gilbert, R-devel, 2003-06-18
z <- as.ts(matrix(rnorm(100), , 1))
class(z) # not "mts"
is.matrix(z) # TRUE in 1.7.1
pacf(z)
pacf(matrix(rnorm(100), , 1))
## both failed in 1.7.1.


## lsfit was not setting residuals in the rank=0 case
fit <- lsfit(matrix(0, 10, 1), 1:10, intercept=FALSE)
stopifnot(fit$residuals == 1:10)
## zero residuals in 1.7.1.


## interval calculations on predict.lm
x <- 1:10
y <- rnorm(10)
predict(lm(y ~ x), type="terms", interval="confidence")
##


## 0-level factors
f <- factor(numeric(0))
sort(f)
unique(f)
## both failed in 1.7.1


## data failed with some multiple inputs
data(cars, women)
## failed in 1.7.1


## body() and formals() looked in different places
bar <- function(x=NULL)
{
   foo <- function(y=3) testit()
   print(formals("foo"))
   print(body("foo"))
}
bar()
## the call to body() failed in 1.7.0


## string NAs shouldn't have any internal structure.(PR#3078)
a <- c("NA", NA, "BANANA")
na <- as.character(NA)
a1 <- substr(a,1,1)
stopifnot(is.na(a1)==is.na(a))
a2 <- substring(a,1,1)
stopifnot(is.na(a2)==is.na(a))
a3 <- sub("NA","na",a)
stopifnot(is.na(a3)==is.na(a))
a3 <- gsub("NA","na",a)
stopifnot(is.na(a3)==is.na(a))
substr(a3, 1, 2) <- "na"
stopifnot(is.na(a3)==is.na(a))
substr(a3, 1, 2) <- na
stopifnot(all(is.na(a3)))
stopifnot(agrep("NA", a) == c(1, 3))
stopifnot(grep("NA", a) == c(1, 3))
stopifnot(grep("NA", a, perl=TRUE) == c(1, 3))
stopifnot(all(is.na(agrep(na, a))))
stopifnot(all(is.na(grep(na, a))))
stopifnot(all(is.na(grep(na, a, perl=TRUE))))
a4 <- abbreviate(a)
stopifnot(is.na(a4) == is.na(a))
a5 <- chartr("NA", "na", a)
stopifnot(is.na(a5) == is.na(a))
a6 <- gsub(na, "na", a)
stopifnot(all(is.na(a6)))
a6a <- gsub("NANA", na, a)
stopifnot(is.na(a6a)==c(FALSE, TRUE, TRUE))
a7 <- a; substr(a7, 1, 2) <- "na"
stopifnot(is.na(a7) == is.na(a))
a8 <- a; substr(a8, 1, 2) <- na
stopifnot(all(is.na(a8)))
stopifnot(identical(a, toupper(tolower(a))))
a9<-strsplit(a, "NA")
stopifnot(identical(a9, list("",na,c("BA",""))))
a10<-strsplit(a, na)
stopifnot(identical(a10, as.list(a)))
## but nchar doesn't fit this pattern
stopifnot(all(!is.na(nchar(a))))
## NA and "NA" were not distinguished in 1.7.x


## coercing 0-length generic vectors
as.double(list())
as.integer(list())
as.logical(list())
as.complex(list())
as.character(list())
## all but the last failed in 1.7.x


## help on reserved words
## if else repeat while function for in next break  will fail
if(.Platform$OS.type == "windows") options(pager="console")
for(topic in c("TRUE", "FALSE",  "NULL", "NA", "Inf", "NaN")) {
    eval(parse(text=paste("?", topic, sep="")))
    eval(parse(text=paste("help(", topic, ")", sep="")))
}
## ?NULL and all the help calls fail in 1.7.x


## row names in data frames
xx <- structure(1:3, names=letters[1:3])
data.frame(xx)
data.frame(xx, yy=1:6) # failed with misleading message in 1.7.x
data.frame(xx, yy=1:6, row.names=NULL) # no warning
##


## empty paste
stopifnot(length(paste(character(0), character(0))) == 0) # was ""
stopifnot(identical(paste(character(0), character(0), collapse="+"), ""))
##


## concatenation of make.names (Tom Minka, R-help, 2003-06-17)
a1 <- make.names(c("a", "a", "a"), unique=TRUE)
a2 <- make.names(c(make.names(c("a", "a"), unique=TRUE), "a"), unique=TRUE)
stopifnot(identical(a1, a2))

df1 <- rbind(data.frame(x=1), data.frame(x=2), data.frame(x=3))
df2 <- rbind(rbind(data.frame(x=1), data.frame(x=2)), data.frame(x=3))
stopifnot(identical(df1, df2))
##


## PR#3280 data.frame(check.name=FALSE) was not always respected
DF <- data.frame(list("a*" = 3), check.names = FALSE)
stopifnot(identical(names(DF), "a*"))
## gave "a." in 1.7.1


## functions using get() were not always looking for functions or in the
## right place
x <- factor(1:3)
contrasts(x) <- "ctr"
test <- function(x)
{
    ctr <- contr.treatment
    contrasts(x)  # failed in 1.7.1
}
test(x)
##

## get/exists were ignoring mode in base
stopifnot(exists(".Device"))
stopifnot(!exists(".Device", mode="function")) # was true in 1.7.1
##


## inadvertent recursive indexing bug (PR#3324)
x <- list(a=1:3, b=2:4)
try(x[[c("c", "d")]])
try(x[[c("c", "d")]] <- NA)
## both segfaulted in 1.7.1


## empty indexing of data frames  (PR#3532)
x <- data.frame(x = "1.5")
num <- numeric(0)
x[num] <- list()
x[, num] <- list()
## x[[num]] is rightly an error
## x[num] etc failed in 1.7.x.


## .Random.seed was searched for with inherits=TRUE
rm(.Random.seed)
attach(list(.Random.seed=c(0:4)))
runif(1)
detach(2)
(new <- RNGkind())
stopifnot(identical(new, c("Mersenne-Twister", "Inversion")))
stopifnot(identical(find(".Random.seed"), ".GlobalEnv"))
## took from and assigned to list in 1.7.x.


## PR#3750
y <- c(1, NA, NA, 7)
identical(y, qqnorm(y, plot.it=FALSE)$y)
## qqnorm() used to drop NA's in its result till 1.7.x


## PR#3763
d0 <- ISOdate(2001,1,1)[0] # length 0 POSIX
(rd0 <- round(d0, "day"))
stopifnot(identical(rd0, as.POSIXlt(d0)))
## 2nd line gave floating point exception (in format(*)!)


## New det() function
stopifnot(det(m <- cbind(1, c(1, 1))) == 0,
          determinant(m           )$mod == -Inf,
          determinant(m, log=FALSE)$mod == 0)
## gave error for singular matrices in earlier Aug.2003


## tests of model fitting in the presence of non-syntactic names
names(swiss)[6] <- "Infant Mortality"
(lm1 <- lm(Fertility ~ ., data = swiss))
formula(lm1) # is expanded out
slm1 <- step(lm1)
add1(lm1, ~ I(Education^2) + .^2)
step(lm1, scope=~ I(Education^2) + .^2)

Quine <- structure(list(Eth = structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2), .Label = c("A", "N"), class = "factor"),
    Sex = structure(c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Label = c("F",
    "M"), class = "factor"), Age = structure(c(1, 1, 1, 1, 1,
    1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4,
    4, 4, 4, 4, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4), .Label = c("F0", "F1", "F2", "F3"
    ), class = "factor"), Lrn = structure(c(2, 2, 2, 1, 1, 1,
    1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2,
    2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1), .Label = c("AL", "SL"), class = "factor"),
    Days = c(2, 11, 14, 5, 5, 13, 20, 22, 6, 6, 15, 7, 14, 6,
    32, 53, 57, 14, 16, 16, 17, 40, 43, 46, 8, 23, 23, 28, 34,
    36, 38, 3, 5, 11, 24, 45, 5, 6, 6, 9, 13, 23, 25, 32, 53,
    54, 5, 5, 11, 17, 19, 8, 13, 14, 20, 47, 48, 60, 81, 2, 0,
    2, 3, 5, 10, 14, 21, 36, 40, 6, 17, 67, 0, 0, 2, 7, 11, 12,
    0, 0, 5, 5, 5, 11, 17, 3, 4, 22, 30, 36, 8, 0, 1, 5, 7, 16,
    27, 0, 30, 10, 14, 27, 41, 69, 25, 10, 11, 20, 33, 5, 7,
    0, 1, 5, 5, 5, 5, 7, 11, 15, 5, 14, 6, 6, 7, 28, 0, 5, 14,
    2, 2, 3, 8, 10, 12, 1, 1, 9, 22, 3, 3, 5, 15, 18, 22, 37)),
          .Names = c("Eth", "Sex", "Age", "Slow or fast", "Days"),
          class = "data.frame",
          row.names = c("1",
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
"14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
"25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
"36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46",
"47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57",
"58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68",
"69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
"80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
"91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
"101", "102", "103", "104", "105", "106", "107", "108", "109",
"110", "111", "112", "113", "114", "115", "116", "117", "118",
"119", "120", "121", "122", "123", "124", "125", "126", "127",
"128", "129", "130", "131", "132", "133", "134", "135", "136",
"137", "138", "139", "140", "141", "142", "143", "144", "145",
"146"))
step(aov(log(Days+2.5) ~ .^4, data=Quine))
DF <- data.frame(y=rnorm(21), `x 1`=-10:10., check.names = FALSE)
lm(y ~ ., data = DF)
(fm <- lm(y ~ `x 1` + I(`x 1`^2), data = DF))
step(fm)

N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,55.0,
           62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)
npk <- data.frame(`block no`=gl(6,4), N=factor(N), P=factor(P),
                  K=factor(K), yield=yield, check.names=FALSE)
op <- options(contrasts=c("contr.helmert", "contr.treatment"))
(npk.aovE <- aov(yield ~  N*P*K + Error(`block no`), npk))
summary(npk.aovE)
model.tables(npk.aovE)
model.tables(npk.aovE, "means")
options(op)# reset to previous
## Didn't work before 1.8.0


library(stats)
## cmdscale
## failed in versions <= 1.4.0 :
cm1 <- cmdscale(eurodist, k=1, add=TRUE, x.ret = TRUE)
cmdsE <- cmdscale(eurodist, k=20, add = TRUE, eig = TRUE, x.ret = TRUE)
# FAILED on Debian testing just prior to 1.9.0!
#stopifnot(identical(cm1$x,  cmdsE$x),
#          identical(cm1$ac, cmdsE$ac))
stopifnot(all.equal(cm1$x,  cmdsE$x),
          all.equal(cm1$ac, cmdsE$ac))
## end of moved from cmdscale.Rd


## cutree
hc <- hclust(dist(USArrests))
ct <- cutree(hc, h = c(0, hc$height[c(1,49)], 1000))
stopifnot(ct[,"0"]== 1:50,
          unique(ct[,2]) == 1:49,
          ct[,3]  == ct[,4],
          ct[,4]  == 1)
## end of moved from cutree.Rd


## princomp
USArrests[1, 2] <- NA
pc.cr <- princomp(~ Murder + Assault + UrbanPop,
                  data = USArrests, na.action=na.exclude, cor = TRUE)
update(pc.cr, ~ . + Rape)
## end of moved from princomp.Rd


## smooth.spline.Rd
y18 <- c(1:3,5,4,7:3,2*(2:5),rep(10,4))
xx  <- seq(1,length(y18), len=201)
s2. <- smooth.spline(y18, cv=TRUE,con=list(trace=TRUE, tol=1e-6,low= -3,maxit=20))
s2. ## Intel-Linux: Df ~= (even! > ) 18 : interpolating -- much smaller PRESS
## {others, e.g., may end quite differently!}
lines(predict(s2., xx), col = 4)
mtext(deparse(s2.$call,200), side= 1, line= -1, cex= 0.8, col= 4)

sdf8 <- smooth.spline(y18, df = 8, con=list(trace=TRUE))
sdf8 ; sdf8$df - 8

try(smooth.spline(y18, spar = 50)) #>> error : spar 'way too large'
## end of moved from smooth.spline.Rd


library(ts)
## arima{0}
(fit <- arima(lh, c(1,0,0)))
tsdiag(fit)
(fit <- arima0(lh, c(1,0,0)))
tsdiag(fit)
## end of moved from arima{0}.Rd


## predict.arima
predict(arima(lh, order=c(1,0,1)), n.ahead=5)
predict(arima(lh, order=c(1,1,0)), n.ahead=5)
predict(arima(lh, order=c(0,2,1)), n.ahead=5)
## end of moved from predict.arima.Rd


library(splines)
## ns
## Consistency:
x <- c(1:3,5:6)
stopifnot(identical(ns(x), ns(x, df = 1)),
          !is.null(kk <- attr(ns(x), "knots")),# not true till 1.5.1
          length(kk) == 0)
## end of moved from ns.Rd


## predict.bs
## Consistency:
basis <- ns(women$height, df = 5)
newX <- seq(58, 72, len = 51)
wh <- women$height
bbase <- bs(wh)
nbase <- ns(wh)
stopifnot(identical(predict(basis), predict(basis, newx=wh)),
          identical(predict(bbase), predict(bbase, newx=wh)),
          identical(predict(nbase), predict(nbase, newx=wh)))
## end of moved from predict.bs.Rd


## internal coerceVector() was too lenient
plot(1)
r <- try(strwidth(plot))## Error: cannot coerce
stopifnot(inherits(r, "try-error"),
          grep("cannot coerce", r) == 1)
## gave seg.fault or memory allocation error before 1.8.0


## rank sometimes kept and sometimes dropped names
x2 <- c(3, 1, 4, 1, 5, NA, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:12]
(y1 <- rank(x2))
(y2 <- rank(x2, na.last=FALSE))
(y3 <- rank(x2, na.last=NA))
(y4 <- rank(x2, na.last="keep"))
stopifnot(identical(names(y1), names(x2)),
          identical(names(y2), names(x2)),
          identical(names(y4), names(x2)),
          identical(names(y3), names(x2)[-6]))
##

## as.dist(x) only obeyed `diag=TRUE' or `upper=TRUE' when x was "dist" already
m <- as.matrix(dist(matrix(rnorm(100), nrow=5)))
stopifnot(identical(TRUE, attr(as.dist(m, diag=TRUE), "Diag")))
## failed previous to 1.8.0

stopifnot(1:2 == ave(1:2,factor(2:3,levels=1:3)))
## gave "2 NA" previous to 1.8.0, because unused levels weren't dropped


## PR#4092: arrays with length(dim(.)) = 1
z <- array(c(-2:1, 1.4),5)
cz <- crossprod(as.vector(z))
dimnames(z) <- list(letters[1:5])
z0 <- z
names(dimnames(z)) <- "D1"
stopifnot(crossprod(z) == cz,# the first has NULL dimnames
          identical(crossprod(z), crossprod(z0)),
          identical(crossprod(z), crossprod(z,z0)))
## crossprod(z) segfaulted (or gave silly error message) before 1.8.0


## PR#4431
stopifnot(!is.na(rmultinom(12,100, c(3, 4, 2, 0,0))))
## 3rd line was all NA before 1.8.0


## PR#4275: getAnywhere with extra "."
g0 <- getAnywhere("predict.loess")
g1 <- getAnywhere("as.dendrogram.hclust")
g2 <- getAnywhere("predict.smooth.spline")
g3 <- getAnywhere("print.data.frame")
is.S3meth <- function(ga) any(substr(ga$where, 1,20) == "registered S3 method")
stopifnot(is.S3meth(g0), is.S3meth(g1),
          is.S3meth(g2), is.S3meth(g3))
## all but g0 failed until 1.8.0 (Oct 6)


## symnum(x) for length 0 and some logical arrays:
sm <- symnum(m <- matrix(1:8 %% 3 == 0, 2))
stopifnot(identical(symnum(FALSE[FALSE]), noquote(""[FALSE])),
          identical(symnum(c(m)), c(symnum(m))),
          dim(sm) == dim(m), class(sm) == "noquote")
## symnum(<length 0>) gave noquote("()") before 1.8.1


## abbreviate with leading (or trailing) space differences (PR#4564)
abbreviate(c("A"," A"), 4)
## this gave infinite loop before 1.8.1


## crossprod on 0-extent matrices
a <- matrix(,0,5)
stopifnot(crossprod(a) == 0)
stopifnot(crossprod(a,a) == 0)
stopifnot(crossprod(a+0i) == 0+0i)
## were random areas in <= 1.8.0


## DF[[i, j]] should be row i, col j
stopifnot(women[[2, 1]] == women[2, 1])
women[[2, 1]] <- 77
stopifnot(women[2, 1] == 77)
## was reversed from May 2002 to Oct 2003


## merge.data.frame with a single-column df (PR#4299)
x <- data.frame(x = 1:5, y = letters[1:5])
y <- data.frame(z = 1:2)
z <- merge(x, y)
stopifnot(identical(names(z), c("x", "y", "z")))
## third name was wrong in 1.8.0


## cor(mat, use = "pair") was plainly wrong
# longley has no NA's -- hence all "use = " should give the same!
X <- longley
ep <- 32 * .Machine$double.eps
for(meth in eval(formals(cor)$method)) {
    cat("method = ", meth,"\n")
    Cl <- cor(X, method = meth)
    stopifnot(all.equal(Cl, cor(X, method= meth, use= "complete"), tol=ep),
              all.equal(Cl, cor(X, method= meth, use= "pairwise"), tol=ep),
              all.equal(Cl, cor(X, X, method= meth), tol=ep),
              all.equal(Cl, cor(X, X, method= meth, use= "pairwise"), tol=ep),
              all.equal(Cl, cor(X, X, method= meth, use= "pairwise"), tol=ep)
              )
}
## "pairwise" failed in 1.8.0


## regexpr(*, fixed=TRUE) had 0-index from C
txt <- c("english", "french", "swiss")
ir <- regexpr("en", txt, fixed = TRUE)
stopifnot(ir == c(1, 3, -1),
          identical(ir, regexpr("en", txt)))
## (*, fixed=TRUE) gave 0 2 -1 before R 1.8.1


## PR#5017: filter(init=) had the wrong time order
xx <- filter(4:8, c(1, 0.5, 0.25), method="recursive", init=3:1)
stopifnot(identical(xx[1:3], c(8.25, 15.25, 26.125)))
## 1.8.0 gave 6.75 12.75 22.375


## PR#5090 user error with writeChar could segfault
tf <- tempfile()
zz <- file(tf, "wb")
writeChar("", zz, nchars=10000000)
close(zz)
unlink(tf)
## segfaults in 1.8.0


## PR#4710 round (and signif) dropped attributes
x <- round(matrix(0, 0, 3))
stopifnot(identical(dim(x), as.integer(c(0, 3))))
## numeric(0) in 1.8.0


## PR#5405
try(stepfun(c(), 1)(2))# > Error
## segfaults in 1.8.1 and earlier


## PR#4955 now allow embedded newlines in quoted fields in read.table
temp <- tempfile()
data <- data.frame(a=c("c", "e\nnewline"))
write.table(data, sep=",", row.names=FALSE, file=temp)
data2 <- read.csv(temp)
unlink(temp)
# attributes get a different order here
stopifnot(identical(data$a, data2$a))
## not allowed prior to 1.9.0


## scoping problems with model.frame methods
foo <- c(1,1,0,0,1,1)
rep <- 1:6
m <- lm(foo ~ rep, model=FALSE)
model.matrix(m)
n <- 1:6
m <- lm(foo ~ n, model=FALSE)
model.matrix(m)
## failed in 1.8.0 because the wrong n or rep was found.
rm(foo, rep)
func <- function()
{
    foo <- c(1,1,0,0,1,1)
    rep <- 1:6
    m <- lm(foo ~ rep, model=FALSE)
    model.matrix(m)
}
func()
##


## broken strptime in glibc (and code used on Windows)
# the spec says %d is allowed in 1-31, but it seems HP-UX thinks
# the date is invalid.
# stopifnot(!is.na(strptime("2003-02-30", format="%Y-%m-%d")))
stopifnot(is.na(strptime("2003-02-35", format="%Y-%m-%d")))
# this one is still wrong in glibc
stopifnot(is.na(strptime("2003-02-40", format="%Y-%m-%d")))
stopifnot(is.na(strptime("2003-22-20", format="%Y-%m-%d")))
# and so is this one
stopifnot(is.na(strptime("2003 22 20", format="%Y %m %d")))
stopifnot(is.na(ISOdate(year=2003, month=22, day=20)))
## several after the first gave non-NA values in 1.8.1 on some broken OSes


## PR#4582 %*% with NAs
stopifnot(is.na(NA %*% 0), is.na(0 %*% NA))
## depended on the BLAS in use.


## PR#4688
reli <- cbind(Si = c(2121, 100, 27, 0),
              av = c(4700, 216, 67, 0),
              Nc = c(6234,2461,502,14))
stopifnot(inherits(try(fisher.test(reli, workspace=2000000)), "try-error"))
## gave p.value = Inf ; now gives FEXACT error 501


## PR#5701
chisq.test(matrix(23171,2,2), simulate=TRUE)
## gave infinite loop in 1.8.1 and earlier


## as.matrix on an all-logical data frame
ll <- data.frame(a = rpois(10,1) > 0, b = rpois(10,1) > 0)
stopifnot(mode(as.matrix(ll)) == "logical")
lll <- data.frame(a = LETTERS[1:10], b = rpois(10,1) > 0)
stopifnot(mode(as.matrix(lll)) == "character")
## both were char before 1.9.0


## outer called rep with a non-generic arg
x <- .leap.seconds[1:6]
outer(x, x, "<")
outer(x, x, "-")
(z <- outer(x, x, "difftime", units="days"))
stopifnot(class(z) == "difftime")
## failed in 1.8.1


## PR#5900 qbinom when probability is 1
stopifnot(qbinom(0.95, 10, 1) == 10)
stopifnot(qbinom(0, 10, 1) == 0)
# and for prob = 0
stopifnot(qbinom(0.95, 10, 0) == 0)
stopifnot(qbinom(0, 10, 0) == 0)
# and size = 0
stopifnot(qbinom(0.95, 0, 0.5) == 0)
## 1.8.1 was programmed to give NaN


## base:: and ::: were searching in the wrong places
stopifnot(inherits(try(base::lm), "try-error"))
stopifnot(inherits(try(graphics::log), "try-error"))
## equivalent constructs succeeded in 1.8.1


## (PR#6452) princomp prediction without specifying centers should give NAs
x <- matrix(rnorm(400), ncol=4)
fit <- princomp(covmat=cov(x))
stopifnot(is.null(fit$scores))
stopifnot(is.na(predict(fit, newdata=x[1:10, ])))
## failed in 1.8.1


## (PR#6451) regex functions did not coerce args to character.
sub(x=NA, pattern="x", replacement="y")
## failed in 1.8.1


## length<- needed a factor method, and so needed to be generic
aa <- factor(letters)
length(aa) <- 20
aa
stopifnot(is.factor(aa))
## returned a vector in 1.8.1


## spec.pgram() was too
pAR <- c(2.7607, -3.82, 2.6535, -0.9238)
N <- 1 + 2^14# 16385
set.seed(123)
x <- arima.sim(model = list(ar = pAR), n = N)
spP <- spec.pgram(x, spans = 41, plot=FALSE)
spA <- spec.ar(x=list(ar=pAR, order=4, var.pred=1, frequency=1),
               n.freq = spP$n.used %/% 2, plot=FALSE)
r <- spP$spec / spA$spec
stopifnot(abs(mean(r) - 1) < 0.003)
## was 0.0268 in R 1.8.1


## check for a Microsoft bug in timezones ahead of GMT
stopifnot(!is.na(as.POSIXct("1970-01-01 00:00:00")))
##


## PR#6672, split.default on factors
x <- c(NA, 1, 2)
y <- as.factor(x)
split(x, y)
split(y, y) # included NAs in 1.8.1
r1 <- tapply(x, y, length)
r2 <- tapply(y, y, length)
stopifnot(r1 == r2)
##


## PR#6652, points.formula with subset and extra arguments.
roller <-
    data.frame(weight = c(1.9, 3.1, 3.3, 4.8, 5.3, 6.1, 6.4, 7.6, 9.8, 12.4),
               depression = c(2, 1, 5, 5, 20, 20, 23, 10, 30, 25))
plot(depression ~ weight, data=roller, type="n")
with(roller, points( depression~weight, subset=8:10, col=2))
with(roller, points( depression~weight, subset=8:10, col=2:4))
plot(depression ~ weight, data=roller, type="n")
points(depression~weight, subset=8:10, col=2:4, data=roller)
## first two gave error in 1.8.1


## PR#4558 part 2
x <- seq(as.POSIXct("2004-03-25"), as.POSIXct("2004-03-31"), by="DSTdays")
stopifnot(length(x) == 7)
## was length 6 in the UK time zone.


## PR#6702 c/rbind on list matrices
A <- matrix(as.list(1:4), 2, 2)
(res <- cbind(A, A))
stopifnot(typeof(res) == "list")
(res <- rbind(A, A))
stopifnot(typeof(res) == "list")
## were not implemented in 1.8.1


## Date objects with NA's
(t1 <- strptime(c("6. Aug. 1930", "3. Nov. 1925", "28. Mar. 1959",
                 NA, paste(1:29," Feb. 1960", sep=".")),
               format = "%d. %b. %Y"))
stopifnot(6 == length(print(s1 <- summary(t1))),
          s1== summary(as.POSIXct(t1)),
          6 == length(print(format(as.Date(s1)))) )
## gave bizarre "NA's" entry in R 1.8.1 and 1.9.0alpha


## as.Date on a factor
as.Date(factor("2000-01-02"))
## failed in 1.9.0


## as.data.frame.list (PR#6782)
xx <- list(row.names=1:2,foxglove=3:4,toadflax=5:6)
foo <- as.data.frame(xx)
stopifnot(identical(names(xx), names(foo)))
## 1.9.0 changed the last name to "x".


## type.convert quirk (PR#6781)
res1 <- type.convert( c("abc","-"), as.is=TRUE, na.strings="-" )
stopifnot(identical(mode(res1), "character"), is.na(res1[2]))
## res1[2] was "-" <= 1.9.0.


## subsetting factor swaps order of attributes (PR#6799)
af <- factor(c('A','B'))
stopifnot(identical(af, af[1:2]))
## failed in 1.9.0 as the attributes were class, level for af[1:2]


## Comparison between lists and expressions
stopifnot(inherits(try(list(1) <= list(2)), "try-error"))
e <- expression(3 + 2 * 4)
stopifnot(inherits(try(e == e), "try-error"))
## both were allowed but nonsense in 1.9.0


## "nowhere" interpolation (PR#6809)
try(approx(list(x=rep(NaN, 9), y=1:9), xout=NaN))
## gave a seg.fault in 1.9.0


## aggregate.data.frame failed if result would have one row
## Philippe Hup�, R-help, 2004-05-14
dat <- data.frame(a=rep(2,10),b=rep("a",10))
aggregate(dat$a, by=list(a1=dat$a, b1=dat$b), NROW)
## failed due to missing drop = FALSE


## [<-.data.frame with a data-frame value
x <- data.frame(a=1:3, b=4:6, c=7:9)
info <- x[1:2]
x[, names(info)] <-  info[1,]
##


## invalid 'lib.loc'
stopifnot(length(installed.packages("mgcv")) == 0)
## gave a low-level error message


## as.dendrogram.hclust()
d <- as.dendrogram(hEU <- hclust(eurodist, "ave"))
stopifnot(order.dendrogram(d) == hEU$order)# not new
##N require(gclus); hE1 <- reorder.hclust(hEU, dis)
## reconstruct without gclus (for R's testing)
hE2 <- hEU; ii <- c(5,9:11, 13, 15:18); hE2$merge[ii,] <- hEU$merge[ii, 2:1]
hE2$order <- as.integer(c(1,19,9,12,14,2,15,8,13,16,17,21,6,3,11,4,18,5,10,7,20))
##N stopifnot(identical(hE1, hE2))
d1 <- as.dendrogram(hE2)
stopifnot(order.dendrogram(d1) == hE2$order,
          identical(d1, rev(rev(d1))))
## not true in 1.9.0


## trunc on a Date
trunc(xx <- Sys.Date()) # failed in 1.9.1
x <- xx + 0.9
stopifnot(identical(trunc(x), xx)) # gave next day in 1.9.1
xx <- as.Date("1960-02-02")
x <- xx + 0.2
stopifnot(identical(trunc(x), xx)) # must not truncate towards 0.
##

### end of tests added in 1.9.1 ###

## 1.9.1 patched

## options(list('..', '..'))
try(options(list('digits', 'width')))# give an error
## gave a segfault in 1.9.1


## PR#7100 seg faulted or path too long error on ././././././. ...
list.files('.', all.files = TRUE, recursive = TRUE)


## PR#7116 seg faulted :
cor(as.array(c(a=1,b=2)), cbind(1:2))


## regression test for PR#7108
ans <- gsub(" ", "", "b c + d | a * b", perl=TRUE) # NULL in 1.9.1
stopifnot(identical(ans, gsub(" ", "", "b c + d | a * b")))
gsub(" ", "", "a: 12345 :a", perl=TRUE) # segfaulted in 1.9.1
## wrong answers, segfaults in 1.9.1.


## regression test for PR#7132
tmp <- data.frame(y=rnorm(8),
                  aa=factor(c(1,1,1,1,2,2,2,2)),
                  bb=factor(c(1,1,2,2,1,1,2,2)),
                  cc=factor(c(1,2,3,4,1,2,3,4)))
tmp.aov <- aov(y ~ cc + bb/aa, data=tmp)
anova(tmp.aov)
model.tables(tmp.aov, type="means")
## failed in 1.9.1.

if(require(survival)) { # required package
  a <- Surv(1:4, 2:5, c(0,1,1,0))
  str(a)
  str(a[rep(1:4,3)], vec.len = 7)
  detach("package:survival")
}

### end of tests added in 1.9.1 patched ###


## names in columns of data frames
x <- 1:10
names(x) <- letters[x]
DF <- data.frame(x=x)
(nm <- names(DF$x))
stopifnot(is.null(nm))
DF$y1 <- x
DF["y2"] <- x
DF[, "y3"] <- x
DF[["y4"]] <- x
stopifnot(is.null(names(DF$y1)), is.null(names(DF$y2)),
          is.null(names(DF$y3)), is.null(names(DF$y4)))
# names were preserved in 1.9.x
# check factors
xx <- as.factor(x)
DF <- data.frame(x=xx)
(nm <- names(DF$xx))
stopifnot(is.null(nm))
DF$y1 <- xx
DF["y2"] <- xx
DF[, "y3"] <- xx
DF[["y4"]] <- xx
stopifnot(is.null(names(DF$y1)), is.null(names(DF$y2)),
          is.null(names(DF$y3)), is.null(names(DF$y4)))
# how about AsIs?  This should preserve names
DF <- data.frame(x=I(x))
(nm <- names(DF$x))
stopifnot(identical(nm, names(x)))
DF2 <- rbind(DF, DF[7:8,, drop=FALSE])
(nm <- names(DF2$x))
stopifnot(identical(nm, c(names(x), names(x)[7:8])))
# and matrices?  Ordinary matrices will be split into columns
x <- 1:10
dim(x) <- c(5,2)
dimnames(x) <- list(letters[1:5], c("i", "ii"))
DF <- data.frame(x=I(x))
DF2 <- rbind(DF, DF)
(rn <- rownames(DF2$x))
stopifnot(identical(rn, c(rownames(x), rownames(x))))
class(x) <- "model.matrix"
DF <- data.frame(x=x)
DF2 <- rbind(DF, DF)
(rn <- rownames(DF2$x))
stopifnot(identical(rn, c(rownames(x), rownames(x))))
## names were always preserved in 1.9.x, but rbind dropped names and dimnames.


## cumsum etc dropped names
x <- rnorm(10)
names(x) <- nm <- letters[1:10]
stopifnot(identical(names(cumsum(x)), nm),
          identical(names(cumprod(x)), nm),
          identical(names(cummax(x)), nm),
          identical(names(cummin(x)), nm))
x <- x+1i
stopifnot(identical(names(cumsum(x)), nm),
          identical(names(cumprod(x)), nm))
## 1.9.x dropped names


## complex superassignments
e <- c(a=1, b=2)
f <- c(a=1, b=2)
g <- e
h <- list(a=1, list(b=2, list(c=3, d=4), list(e=5)))
j <- matrix(1, 2, 2)
a <- "A"
local({
  eold <- e <- c(A=10, B=11)
  hold <- h <- 2
  jold <- j <- 7
  gold <- g <- e
  a <- "B"

  e[2] <<- e[2]+1
  names(f)[2] <<- a
  g <<- 1
  h[[2]][[h]][[ f[e==10] ]] <<- h
  names(h[[2]][[h]])[f[e==10] ] <<- a
  j[h, h] <<- h
  colnames(j)[2] <<- a

  stopifnot(identical(e, eold))
  stopifnot(identical(h, hold))
  stopifnot(identical(g, gold))
  stopifnot(identical(j, jold))
})

stopifnot(identical(e, c(a=1, b=12)))
stopifnot(identical(f, c(a=1, B=2)))
stopifnot(identical(g, 1))
stopifnot(identical(h, list(a=1, list(b=2, list(B=2, d=4), list(e=5)))))
stopifnot(identical(as.vector(j), c(1, 1, 1, 2)))
stopifnot(identical(colnames(j), c(NA,"B")))
## gave error 'subscript out of bounds' in 1.9.1

## make sure we don't get cycles out of changes to subassign3.
x <- list(a=1, y=2)
x$a <- x
print(x)
x$d <- x
print(x)
y <- x
x$b <- y
print(x)
x$f <- y
print(x)
##


## model.frame incorrectly preserved ts attributes
x1 <- ts(c(1:10, NA))
y1 <- ts(rnorm(11))
lm(y1 ~ x1)
lm(y1 ~ x1 + I(x1^2)) # second term has two classes
## failed in 1.9.1


## range checks missing in recursive assignments (PR#7196)
l <- list()
try(l[[2:3]] <- 1)
l <- list(x=2)
try(l[[2:3]] <- 1)
l <- list(x=2, y=3)
l[[2:3]] <- 1
## first two segfaulted in 1.9.x


## apply() on an array of dimension >=3 AND when for each iteration
## the function returns a named vector of length >=2 (PR#7205)
a <- array(1:24, dim=2:4)
func1 <- function(x) c(a=mean(x), b=max(x))
apply(a, 1:2, func1)
## failed in 1.9.1


# col2rgb must return a matrix for a single colour
stopifnot(is.matrix(col2rgb("red")))
## was vector at one point in pre-2.0.0


## Subscripting matrices with NA's
AAA <- array(1:6, c(6,1,1))
idx <- c(1,2,NA,NA,5,6)
B <- 10
AAA[idx,1,1] <- B
stopifnot(all.equal(as.vector(AAA), c(10,10,3,4,10,10)))
## assigned only the first two elements in 1.9.1.
## Tests for >= 2.0.0
A <- c(1,2,3,4,5,6)
A[idx] <- 27 # OK, one value
stopifnot(identical(A, c(27,27,3,4,27,27)))
try(A[idx] <- 6:1) # was 6 5 3 4 2 1 in 1.9.1
stopifnot(inherits(.Last.value, "try-error"))

AA <- matrix(c(1,2,3,4,5,6), 6, 1)
AA[idx,] <- 27 # OK, one value
stopifnot(identical(AA, matrix(c(27,27,3,4,27,27), 6, 1)))
try(AA[idx,] <- 6:1) # was 6 5 3 4 4 3 in 1.9.1
stopifnot(inherits(.Last.value, "try-error"))

AAA <- array(c(1,2,3,4,5,6), c(6,1,1))
AAA[idx,,] <- 27 # OK, one value
stopifnot(identical(AAA, array(c(27,27,3,4,27,27), c(6,1,1))))
try(AAA[idx,,] <- 6:1) # was 6 5 3 4 5 6 in 1.9.1
stopifnot(inherits(.Last.value, "try-error"))
## only length-1 values are allowed in >= 2.0.0.


## hist with infinite values (PR#7220)
hist(log(-5:100), plot = FALSE)
## failed in 1.9.1: will warn, correctly.


## merge problem with names/not in rbind.data.frame
x <- structure(c("a", "b", "2", "0.2-26", "O", "O"), .Dim = c(2, 3),
               .Dimnames = list(c("1", "2"), c("P", "V", "2")))
y <- structure(c("a", "b", "2", "0.2-25", "O", "O"), .Dim = c(2, 3),
               .Dimnames = list(c("1", "2"), c("P", "V", "1")))
merge(x, y, all.y = TRUE)
## failed for a while in pre-2.0.0


## matrix responses in binomial glm lost names prior to 2.0.0
y <- rbinom(10, 10, 0.5)
x <- 1:10
names(y) <- letters[1:10]
ym <- cbind(y, 10-y)
fit2 <- glm(ym ~ x, binomial)
stopifnot(identical(names(resid(fit2)), names(y)))
## Note: fit <- glm(y/10 ~ x, binomial, weights=rep(10, 10))
## Does not preserve names in R < 2.0.1, but does in S.
fit <- glm(y/10 ~ x, binomial, weights=rep(10, 10))
stopifnot(identical(names(resid(fit)), names(y)))
## The problem was glm.fit assumed a vector response.


## dlogis(-2000) was NaN in <= 2.0.0.
stopifnot(identical(dlogis(-2000), 0.0))
##


## short vectors in spline[fun]  (PR#7290)
try(splinefun(1[0], 1[0])(1)) # segfault in <= 2.0.0
for(meth in c("fmm", "nat", "per"))
    stopifnot(all(splinefun(1, pi, method = meth)(0:2) == rep(pi, 3)))
## exactly constant for n=1; was NA for "periodic" in <= 2.0.0


## ecdf with NAs (part of PR#7292).
x <- c(1,2,2,4,7, NA, 10,12, 15,20)
ecdf(x)
## failed in <= 2.0.0.


## Incorrect use of as.Date segfaulted on some x86_64 systems.
as.Date("2001", "%Y")
## answer is usually current mon & day, but 2001-01-01 on Solaris.


## rank and order accepted invalid inputs (and gave nonsense)
x1 <- as.list(10:1)
x2 <-  charToRaw("A test string")
stopifnot(inherits(try(order(x1)), "try-error"),
          inherits(try(order(x2)), "try-error"),
          inherits(try(rank(x1)), "try-error"),
          inherits(try(rank(x2)), "try-error"))
## worked but gave 1:n in 2.0.0.
stopifnot(inherits(try(sort(x1)), "try-error"),
          inherits(try(sort(x2)), "try-error"),
          inherits(try(sort(x1, partial=5)), "try-error"),
          inherits(try(sort(x2, partial=5)), "try-error"))
##


## pmax failed with NA inputs
pmax(c(1,2,NA), c(3,4,NA), na.rm=TRUE)
## failed after for 2.0.0 change to subassignment


## subassigning expression could segfault (PR#7326)
foo <- expression(alpha, beta, gamma)
foo[2]
foo[2] <- NA
foo
## segfaulted in 2.0.0


## incorrect arg matching in sum min max prod any all
## Pat Burns, R-devel 2004-11-19
stopifnot(identical(sum(1:4, NA, n = 78, na.rm = TRUE), 88))
## was 11 in 2.0.1


## segfault from text, P Ehlers, R-devel 2004-11-24
plot(1:10)
loc <- list(5, 6)
try(text(loc, labels = "a"))
## segfaulted in 2.0.1


## automatic row.names can be number-like, MM, 2004-11-26
d0 <- data.frame(x=1:3, y=pi*2:0)
row.names(d0)[3] <- c("01.00")
write.table(d0, (tf <- tempfile()))
d <- read.table(tf)
## gave error ("duplicate row.names") in 2.0.1
stopifnot(all.equal(d,d0))
unlink(tf)


## seq() should be more consistent in returning "integer"
stopifnot(typeof(seq(length=0)) == "integer",
          identical(seq(length=0), seq(along=0[0])),
          identical(seq(length=3), 1:3),
          identical(seq(length=3), seq(along=1:3)))


## labels.lm was broken (PR#7417)
# part of example(lm)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
stopifnot(labels(lm.D9) == "group")
## failed in 2.0.1, giving length 0


## sprintf had no length check (PR#7554)
a <- matrix (ncol=100, nrow=100, data=c(1,2,3,4,5))
a.serial <- serialize(a, NULL, ascii=TRUE)
try(sprintf('foo: %s\n', a.serial))
## seqfaulted in 2.0.1


## all/any did not coerce as the Blue Book described.
for(x in c("F", "FALSE", "T", "TRUE", "NA")) {
    print(all(x))
    print(any(x))
}
all(list())
any(list())
## all failed in 2.0.1 with 'incorrect argument type'


##---- named dimnames of  %*% and crossprod() -- matrices and 1-d arrays:
tst1 <- function(m) {
    stopifnot(identical(t(m) %*%  (m), crossprod(m)))
    stopifnot(identical(m    %*% t(m), crossprod(t(m))))
}
tst2 <- function(x, y=x) {
    stopifnot(identical(t(x) %*% (y),(crossprod(x,y) ->  C)))
    stopifnot(identical(t(y) %*% (x),(crossprod(y,x) -> tC)))
    stopifnot(identical(tC, t(C)))
}

{m1 <- array(1:2,1:2); dimnames(m1) <- list(D1="A", D2=c("a","b")); m1}
tst1(m1)
m2 <- m1; names(dimnames(m2)) <- c("", "d2"); tst1(m2)
m3 <- m1; names(dimnames(m3)) <- c("", "")  ; tst1(m3)
m4 <- m1; names(dimnames(m4)) <- NULL       ; tst1(m4)

tst2(m1,m2)
tst2(m1,m3)
tst2(m1,m4)
tst2(m2,m3)
tst2(m2,m4)
tst2(m3,m4)

## 2) Now the 'same' with 1-d arrays:
a1 <- m1; dim(a1) <- length(a1); dimnames(a1) <- dimnames(m1)[2]; a1 # named dn
a2 <- a1; names(dimnames(a2)) <- NULL ; a2 # unnamed dn
a3 <- a1; dimnames(a3) <- NULL ; a3 # no dn
stopifnot(identical(dimnames(t(a1))[2], dimnames(a1)))
## in version <= 2.0.1,  t(.) was loosing names of dimnames()
tst1(a1)# failed in 2.0.1 ("twice")
tst1(a2)# failed in 2.0.1
tst1(a3)# ok
## these all three failed in (2.0.1) for more than one reason:
tst2(a1,a2)
tst2(a1,a3)
tst2(a2,a3)
## end {testing named dimnames for %*% and crossprod()}


## -- coercing as.data.frame(NULL) to a pairlist didn't work
y<-1:10
eval(quote(y), as.data.frame(NULL))
## NULL as the second argument of eval should be treated
## like a list or data frame
eval(quote(y), NULL)
## end


## data frame with nothing to replace
A <- matrix(1:4, 2, 2)
A[is.na(A)] <- 0
A <- as.data.frame(A)
A[is.na(A)] <- 0
## last not accepted prior to 2.1.0


## scan on partial lines on an open connection
cat("TITLE extra line", "235 335 535 735", "115 135 175",
    file="ex.data", sep="\n")
cn.x <- file("ex.data", open="r")
res <- scan(cn.x, skip=1, n=2)
res <- c(res, scan(cn.x, n=2))
res <- c(res, scan(cn.x, n=2))
res <- c(res, scan(cn.x, n=2))
close(cn.x, sep=" ")
unlink("ex.data")
stopifnot(identical(res, c(235, 335, 535, 735, 115, 135, 175)))
## dropped some first chars < 2.1.0


## PR#7686 formatC does not pick up on incorrect 'flag' inputs
try(formatC(1, flag="s"))
## segfaulted in 2.0.1


## PR#7695 contrasts needed coercion to double
c <- matrix(c(0,1,2), nrow=3)
storage.mode(c) <- "integer"
f <- factor(1:3)
contrasts(f, 1) <- c
x <- model.matrix(~f)
stopifnot(x == c(1,1,1,0,1,2))
## gave machine-dependendent silly numbers in 2.0.1


## extreme (de-normalized) axis range
x <- 2^-seq(67, 1067, length=20)
plot(x^.9, x, type="l", log="xy") # still warning and ugly labels because
                                 ## e.g., 10^-323 |==> 9.881313e-324 numerically
## gave error "log - axis(), 'at' creation, _LARGE_ range..." in 2.0.1


## torture test of scan() with allowEscape=TRUE
tf <- tempfile()
x <- c('ABC', '"123"', "a'b")
cat(shQuote(x, "cmd"), sep="\n", file=tf)
(x2 <- scan(tf, ""))
unlink(tf)
stopifnot(identical(x, x2))
## At one point pre-2.1.0 got confused


## se.contrast failed in 2.0.1 with some effectively one-stratum designs.
old <- getOption("contrasts")
options(contrasts = c("contr.helmert", "contr.poly"))
Lab <- factor(rep(c("1","2","3"), each=12))
Material <- factor(rep(c("A","B","C","D"),each=3,times=3))
Measurement <- c(12.20,12.28,12.16,15.51,15.02,15.29,18.14,18.08,18.21,
                 18.54,18.36,18.45,12.59,12.30,12.67,14.98,15.46,15.22,
                 18.54,18.31,18.60,19.21,18.77,18.69,12.72,12.78,12.66,
                 15.33,15.19,15.24,18.00,18.15,17.93,18.88,18.12,18.03)
testdata <- data.frame(Lab, Material, Measurement)
(test.aov <- aov(Measurement ~ Material + Error(Lab/Material),
                 data = testdata))
eff.aovlist(test.aov)
(res <- se.contrast(test.aov,
                    list(Material=="A", Material=="B",
                         Material=="C", Material=="D"),
                    coef = c(1, 1, -1, -1), data = testdata))
## failed in 2.0.1 as a matrix was 1 x 1.

## 2.0.1 also failed to check for orthogonal contrasts
## in calculating the efficiencies (which are 1 here).
options(contrasts = c("contr.treatment", "contr.poly"))
(test2.aov <- aov(Measurement ~ Material + Error(Lab/Material),
                  data = testdata))
(res2 <- se.contrast(test2.aov,
                     list(Material=="A", Material=="B",
                          Material=="C", Material=="D"),
                     coef = c(1, 1, -1, -1), data = testdata))
stopifnot(all.equal(res, res2))

## related checks on eff.aovlist
example(eff.aovlist) # helmert contrasts
eff1 <- eff.aovlist(fit)
fit <- aov(Yield ~ A * B * C + Error(Block), data = aovdat)
eff2 <- eff.aovlist(fit)
stopifnot(all.equal(eff1, eff2)) # will have rounding-error differences
options(contrasts = old)
## Were different in earlier versions


## parts of PR#7742 and other examples
sub('^','v_', 1:3, perl=TRUE)
## 2.0.1 did not coerce to character (nor was it documented to).
x <- LETTERS[1:3]
stopifnot(identical(paste('v_', x, sep=""),
                    sub('^','v_', x, perl = TRUE)))
## 2.0.1 added random chars at the end
stopifnot(identical(paste('v_', x, sep=""), sub('^','v_', x)))
## 2.0.1 did not substitute at all
(x <- gsub("\\b", "|", "The quick brown fox", perl = TRUE))
stopifnot(identical(x, "|The| |quick| |brown| |fox|"))
## checked against sed: 2.0.1 infinite-looped.
(x <- gsub("\\b", "|", "The quick brown fox"))
stopifnot(identical(x, "|The| |quick| |brown| |fox|"))
## 2.0.1 gave wrong answer
## Another boundary case,
(x <- gsub("\\b", "|", " The quick "))
stopifnot(identical(x, " |The| |quick| "))
(x <- gsub("\\b", "|", " The quick ", perl = TRUE))
stopifnot(identical(x, " |The| |quick| "))
## and some from a comment in the GNU sed code
x <- gsub("a*", "x", "baaaac")
stopifnot(identical(x, "xbxcx"))
x <- gsub("a*", "x", "baaaac", perl = TRUE)
stopifnot(identical(x, "xbxcx"))
## earlier versions got "bxc" or "xbxxcx"
(x <- gsub("^12", "x", "1212")) # was "xx"
stopifnot(identical(x, "x12"))
(x <- gsub("^12", "x", "1212", perl = TRUE)) # was "xx"
stopifnot(identical(x, "x12"))
## various fixes in 2.1.0

## length(0) "dist":
(d01. <- dist(matrix(0., 0,1)))
## failed in 2.0.1 and earlier


## Wish of PR#7775
x <- matrix(0, nrow=0, ncol=2)
colSums(x); rowSums(x)
x <- matrix(0, nrow=2, ncol=0)
colSums(x); rowSums(x)
## not allowed in 2.0.1


## infinite recursion in 2.0.1 (and R-beta 2005-04-11):
summary(data.frame(mat = I(matrix(1:8, 2))))
summary(data.frame(x = gl(2,2), I(matrix(1:8, 4))))
##

### fixes for 2.1.1 ###

## PR#7792: predict.glm dropped names
nm <- names(predict(glm(y ~ x, family=binomial,
                        data=data.frame(y=c(1, 0, 1, 0), x=c(1, 1, 0, 0))),
                    newdata=data.frame(x=c(0, 0.5, 1)), type="response"))
stopifnot(identical(nm, as.character(1:3)))
## no names in 2.1.0


## PR#7808: as.data.frame: Error in "names<-.default"
x1 <- array(1:9, c(3, 3, 3))
FUN <- function(x1, x2, x3, x4) cbind(x1[, 1, 1:2], x1[, 2, 1:2])[, 1]
as.data.frame(FUN(x1[1:3,,], x2 = c("a", "b"),
                  x3 = c("a", "b"), x4 = c("a", "b")))
## failed in 2.1.0


## PR#7797 citation() chops "Roeland "
stopifnot(as.personList("Roeland Lastname")[[1]]$name[1] == "Roeland")
## was empty in 2.1.0.


## runmed()'s Turlach algorithm seg.faulted in rare cases:
t2 <- c(-2,-7,5,2,-3, 0,1,3,2,-1,2,1,2,1,1,1,-2,4, 1,1,1, 32)
rS <- runmed(t2, k=21, algorithm= "Stuetzle")
rT <- runmed(t2, k=21, algorithm= "Turlach")
stopifnot(identical(rS, rT))
## seg.fault in 2.1.0


## duplicated and unique on a list
x <- list(1, 2, 3, 2)
duplicated(x)
unique(x)
## unique failed in 2.1.0


## prog.aovlist on data with row.names
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
	   55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)
npk <- data.frame(block=gl(6,4), N=factor(N), P=factor(P),
		  K=factor(K), yield=yield)
row.names(npk) <- letters[2:25]
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
pr <- proj(npk.aovE)
## failed in 2.1.0


## PR#7894: Reversing axis in a log plot
x <- 1:3
plot(x, exp(x), log = "y", ylim = c(30,1))
## gave error (and warning) in  log - axis(), 'at' creation

### end of tests added in 2.1.0 patched ###


## Multibyte character set regular expressions had buffer overrun
regexpr("[a-z]", NA)
## crashed on 2.1.1 on Windows in MBCS build.


## PR#8033: density with 'Inf' in x:
d <- density(1/0:2, kern = "rect", bw=1, from=0, to=1, n=2)
stopifnot(all.equal(rep(1/sqrt(27), 2), d$y, tol=1e-14))
## failed in R 2.1.1 (since about 1.9.0)

stopifnot(all.equal(Arg(-1), pi))
## failed in R <= 2.1.1


## PR#7973: reversed log-scaled axis
plot(1:100, log="y", ylim=c(100,10))
stopifnot(axTicks(2) == 10*c(1,2,5,10))
## empty < 2.2.0


## rounding errors in window.default (reported by Stefano Iacus)
x <- ts(rnorm(50001), start=0, deltat=0.1)
length(window(x, deltat=0.4))
length(window(x, deltat=1))
length(window(x, deltat=4.9))
length(window(x, deltat=5))
## last failed in 2.1.1


## incorrect sort in order with na.last != NA
x <- c("5","6",NA,"4",NA)
y <- x[order(x,na.last=FALSE)]
stopifnot(identical(y, c(NA, NA, "4", "5", "6")))
## 2.1.1 sorted "4" first: the fence was wrong.


## integer overflow in cor.test (PR#8087)
n <- 46341
(z <- cor.test(runif(n), runif(n), method = "spearman"))
stopifnot(!is.na(z$p.value))
##

## seek on a file messed up in Windows (PR#7896)
tf <- tempfile()
f <- file(tf, "w+b")
writeChar("abcdefghijklmnopqrstuvwxyz", f, eos=NULL)
seek(f, 0, "end", rw="r")
stopifnot(seek(f, NA, rw="r") == 26) # MinGW messed up seek to end of file that was open for writing
close(f)
f <- file(tf, "rb")
seek(f, 12)
stopifnot(readChar(f, 1) == "m")  # First patch messed up on read-only files
close(f)
unlink(tf)
##

### end of tests added in 2.1.1 patched ###



## tests of hexadecimal constants
x <- 0xAbc
stopifnot(x == 2748)
xx <- as.integer("0xAbc")
stopifnot(x == xx)
xx <- as.numeric("0xAbc")
stopifnot(x == xx)
stopifnot(as.integer("13.7") == 13)
## new in 2.2.0


## save() of raw vector was incorrect on big-endian system
(y <- x <- charToRaw("12345"))
save(x, file="x.Rda")
rm(x)
load("x.Rda")
x
stopifnot(identical(x, y))
unlink("x.Rda")
## 00 00 00 00 00 in 2.1.0 on MacOS X
## fixed for 2.1.1, but test added only in 2.2.x


## PR#7922:  Could not use expression() as an initial expression value
setClass("test2", representation(bar = "expression"))
new("test2", bar = expression())
## failed


## Ops.data.frame had the default check.names=TRUE
DF <- data.frame("100"=1:2, "200"=3:4, check.names=FALSE)
DF/DF
stopifnot(identical(names(DF), names(DF/DF)))
## DF/DF names had X prepended < 2.2.0


## sum(T) was double
x <- 1:10
stopifnot(typeof(sum(x)) == "integer")
x <- c(TRUE, FALSE)
stopifnot(typeof(sum(x)) == "integer")
## double < 2.2.0


## Overflow in PrintGenericVector
x <- paste(1:5000, collapse="+")
as.matrix(list(a=1:2, b=2:3, c=x))
## segfault in 2.1.1, silent truncation in 2.1.1 patched


## weighted.residuals for glm fits (PR#7961)
set.seed(1)
x <- runif(10)
y <- x + rnorm(10)
w <- 0:9
r1 <- weighted.residuals(lm(y ~ x, weights = w))
r2 <- weighted.residuals(glm(y ~ x, weights = w))
stopifnot(all.equal(r1, r2))
## different in 2.1.1


## errors in add1.{lm,glm} when adding vars with missing values(PR#8049)
set.seed(2)
y <- rnorm(10)
x <- 1:10
is.na(x[9]) <- TRUE

lm0 <- lm(y ~ 1)
lm1 <- lm(y ~ 1, weights = rep(1, 10))

add1(lm0, scope = ~ x)
add1(lm1, scope = ~ x)  ## error in 2.1.1

glm0 <- glm(y ~ 1)
glm1 <- glm(y ~ 1, weights = rep(1, 10))
glm2 <- glm(y ~ 1, offset = rep(0, 10))

add1(glm0, scope = ~ x)  ## error in 2.1.1
add1(glm1, scope = ~ x)  ## error in 2.1.1
add1(glm2, scope = ~ x)  ## error in 2.1.1
##


## levels<-.factor dropped other attributes.
## Heinz Tuechler, R-help, 2005-07-18
f1 <- factor(c("level c", "level b", "level a", "level c"), ordered=TRUE)
attr(f1, "testattribute") <- "teststring"
(old <- attributes(f1))
levels(f1) <- c("L-A", "L-B", "L-C")
f1
(new <- attributes(f1))
new$levels <- old$levels <- NULL
stopifnot(identical(old, new))
f2 <- factor(letters[1:4])
levels(f2) <- as.character(c(1:3, NA))
f2
stopifnot(nlevels(f2) == 3)
## dropped other attributes < 2.2.0.


## regressed at one point in pre-2.2.0
A <- matrix(pi, 0, 2)
stopifnot(identical(dim(A), dim(format(A))))
## dropped dim at one point


## ls.diag with missing values (PR#8139)
x <- matrix(c(1,-1,1,-1,1,-1,1,-1,1,-1,  1,2,3,4,5,6,7,8,9,10), 10, 2)
y <- as.matrix(c(1,2,3,NA,3,4,3,4,5,4))
wt <- c(1,1,1,1,1,1,1,1,1,0)
regres <- lsfit(x, y, wt=wt)
regdiag <- ls.diag(regres)
## failed < 2.2.0.


## window.default had an inappropriate tolerance
a <- ts(1:5000, start = 0, freq = 10)
b <- lag(a, 1)
bb <- window(b, start = 0)
stopifnot(length(bb) == length(a) - 1)
## was length(a) - 2 in 2.1.1, since the tolerance was abs(start) * ts.end


## subassignment of length zero vector to NULL gave garbage answer (PR#8157)
x <- NULL
x[[1]] <- numeric(0)
stopifnot(length(x[[1]]) == 0)
## failed < 2.2.0


## some checks for raw in data frames and lists
x <- charToRaw("test")
(z <- data.frame(x))
z$y <- x
z[["y2"]] <- x
z["y3"] <- x
z
## lists use separate code
z <- list(x=x)
z$y <- x
z[["y2"]] <- x
z["y3"] <- list(x)
z
## Not completely supported prior to 2.2.0
