###---- ALL tests here should return  TRUE !
###
###---- "Real" Arithmetic; Numerics etc  -->  ./arith-true.R

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

## factor():
is.factor(factor(list()))
all(levels(ordered(rev(gl(3,4)))) == 1:3)# coercion to char
all(levels(factor(factor(9:1)[3:5])) == 5:7)

### data.frame s :

## Logicals: (S is wrong)
l1 <- c(TRUE,FALSE,TRUE)
(! as.logical(as.data.frame(FALSE)[,1]))
all(l1 == as.logical(as.data.frame(l1)[,1]))
