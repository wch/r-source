###---- ALL tests here should return  TRUE !
###
###---- "Real" Arithmetic; Numerics etc  -->  ./arith-true.R

### mode checking, etc.
is.recursive(expression(1+3, 2/sqrt(pi)))# fix PR#9

## any & all [PR#36]
any(NA,na.rm=TRUE) == any(numeric(0))
all(c(1,NA),na.rm=TRUE) == all(1)

## sum():
all(1:12 == cumsum(rep(1,12)))
x <- rnorm(127); sx <- sum(x);	abs((sum(rev(x)) -sx)) < 1e-12 * abs(sx)

## seq():
typeof(1:4) == "integer" #-- fails for 0.2, 0.3,.., 0.9

all((0:6) == pi + ((-pi):pi))
all((0:7) == (pi+seq(-pi,pi, len=8))*7/(2*pi))

1 == as.integer(is.na(c(pi,NA)[2]))
1 == as.integer(is.nan(0/0))

## rev():
cc <- c(1:10,10:1) ;		all(cc == rev(cc))

## dim[names]():
all(names(c(a=pi, b=1, d=1:4)) == c("a","b", paste("d", 1:4, sep="")))
##P names(c(a=pi, b=1, d=1:4))
ncb <- dimnames(cbind(a=1, yy=1:3))[[2]]
(!is.null(ncb)) && all(ncb == c("a","yy"))

all(cbind(a=1:2, b=1:3, c=1:6) == t(rbind(a=1:2, b=1:3, c=1:6)))
##P rbind(a=1:2, b=1:3, c=1:6)
all(dim(cbind(cbind(I=1,x=1:4), c(a=pi))) == 4:3)# fails in S+

a <- b <- 1:3
all(dimnames(cbind(a, b))[[2]] == c("a","b"))

## rbind PR#338
all(dim(m <- rbind(1:2, diag(2))) == 3:2)
all(m == c(1,1,0, 2,0,1))

## factor():
is.factor(factor(list()))
all(levels(ordered(rev(gl(3,4)))) == 1:3)# coercion to char
all(levels(factor(factor(9:1)[3:5])) == 5:7)
## crossing bug PR#40
is.factor(ff <- gl(2,3) : gl(3,2)) && length(ff) == 6
all(levels(ff) == t(outer(1:2, 1:3, paste, sep=":")))
## from PR#5
ll <- c("A","B"); ff <- factor(ll); f0 <- ff[, drop=TRUE]
all(f0 == ff) && all(levels(ff) == ll) && is.factor(ff) && is.factor(f0)

### data.frame s :

## from lists [bug PR#100]
x <- NULL
x$x1 <- 1:10
x$x2 <- 0:9
all(dim(dx <- as.data.frame(x)) == c(10,2))

## Logicals: (S is wrong)
l1 <- c(TRUE,FALSE,TRUE)
(! as.logical(as.data.frame(FALSE)[,1]))
all(l1 == as.logical(as.data.frame(l1)[,1]))

### Subsetting

## bug PR#425
x <- matrix(1:4, 2, 2, dimnames=list(c("abc","ab"), c("cde","cd")))
y <- as.data.frame(x)
all(x["ab",] == c(2,4))
all(y["ab",] == c(2,4))

## from bug PR#447
x <- 1:2 ; x[c("2","2")] <- 4
all.equal(x, c(1:2, "2" = 4))

## stretching
l2 <- list(a=1, b=2)
l2["cc"] <- pi
l2[["d"]] <- 4
l2 $ e <- 55
all.equal(l2, list(a = 1, b = 2, cc = pi, d = 4, e = 55), tol = 0)
all.equal(l2["d"], list(d = 4))
l2$d == 4 && l2$d == l2[["d"]]

## bug in R <= 1.1
f1 <- y1 ~ x1
f2 <- y2 ~ x2
f2[2] <- f1[2]
deparse(f2) == "y1 ~ x2"

m <- cbind(a=1:2,b=c(R=10,S=11))
all(sapply(dimnames(m), length) == c(2,2))
## [[ for matrix:
m[[1,2]] == m[[3]] && m[[3]] == m[3] && m[3] == m[1,2]

## bug in R <= 1.1.1 : unclass(*) didn't drop the class!
d1 <- rbind(data.frame(a=1, b = I(TRUE)), new = c(7, "N"))
is.null(class(unclass(d1$b)))

## bugs in R 1.2.0
format(as.POSIXct(relR120 <- "2000-12-15 11:24:40")) == relR120
format(as.POSIXct(substr(relR120,1,10))) == substr(relR120,1,10)

require(ts)
## Start new year (i.e. line) at Jan:
(tt <- ts(1:10, start = c(1920,7), end = c(1921,4), freq = 12))
cbind(tt, tt + 1)
