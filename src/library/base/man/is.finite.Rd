\name{is.finite}
\title{Finite, Infinite and NaN Numbers}
\usage{
is.finite(x)
is.infinite(x)
Inf
NaN
is.nan(x)
}
\alias{is.finite}
\alias{is.infinite}
\alias{Inf}
\alias{NaN}
\alias{is.nan}
\description{
\code{is.finite} and \code{is.infinite} return a vector of the same
length as \code{x}, indicating which elements are finite or not.

\code{Inf} and \code{-Inf} are positive and negative `infinity' whereas
\code{NaN} means ``Not a Number''.
}
\details{
\code{is.finite} returns a vector of the same length as \code{x}
the jth element of which is \code{TRUE} if \code{x[j]} is
finite (i.e. it is not one of the values \code{NA}, \code{NaN},
\code{Inf} or \code{-Inf}).

\code{is.infinite} returns a vector of the same length as \code{x}
the jth element of which is \code{TRUE} if \code{x[j]} is
infinite (i.e. equal to one of \code{Inf} or \code{-Inf}).
}
\note{
    In \R, basically all mathematical functions (including basic
    \code{\link{Arithmetic}}), are supposed to work properly with
    \code{+/- Inf} and \code{NaN} as input or output.

    The basic rule should be that calls and relations with \code{Inf}s
    really are statements with a proper mathematical \emph{limit}, see the many
    examples below.
}
\seealso{\code{\link{NA}}, \emph{`Not Available'} which is not a number
    as well, however usually used for missing values.
}
\references{
%%- better ones ?!??
    ANSI/IEEE 754 Floating-Point Standard.

    Currently (6/1999), Bill M.'s \email{billm@melbpc.org.au} tutorial
    and examples at \cr \url{http://www.linuxsupportline.com/~billm/}
}
\examples{
pi / 0 ## = Inf a non-zero number divided by zero creates infinity
0 / 0  ## =  NaN

1/0 + 1/0# Inf
1/0 - 1/0# NaN

stopifnot(
    1/0 == Inf,
    1/Inf == 0
)
exp(-Inf) == 0
##              (actually, the last one seems to give NA on not-very-new
##               versions of Linux, which is a Linux bug and seems to be
##               corrected in newer 'libc6' based Linuxen).

stopifnot(
    is.na(0/0),
    !is.na(Inf),
    is.nan(0/0),

    !is.nan(NA)  &&  !is.infinite(NA)  && !is.finite(NA),
     is.nan(NaN) &&  !is.infinite(NaN) && !is.finite(NaN),
    !is.nan(c(1,NA)),
    c(FALSE,TRUE,FALSE) == is.nan(c   (1,NaN,NA)),
    c(FALSE,TRUE,FALSE) == is.nan(list(1,NaN,NA))#-> FALSE in older versions
)

\testonly{
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
}
lgamma(Inf) == Inf
Inf + Inf == Inf
Inf - Inf == NaN # NA --- should test with  'is.nan()

(1/0) * (1/0)# Inf
(1/0) / (1/0)# NaN

pm <- c(-1,1) # 'pm' = plus/minus

log(0) == - 1/0
exp(-Inf) == 0

sin(Inf)
cos(Inf)
tan(Inf)
all(atan(Inf*pm) == pm*pi/2) # TRUE


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

ix[3] == (iI <- as.integer(Inf))#> warning: NAs introduced by coercion 
ix[4] == (imI<- as.integer(-Inf))
iI  == .Machine$integer.max # TRUE
imI == -.Machine$integer.max # TRUE

##--- Overflow in simple integer arithmetic:
as.integer(2)*iI #  -2
as.integer(3)*iI #  2147483645
as.integer(3)*iI == iI-2 # TRUE

storage.mode(ii <- -3:5)
storage.mode(zm <- outer(ii,ii, FUN="*"))# integer
storage.mode(zd <- outer(ii,ii, FUN="/"))# double
range(zd, na.rm=TRUE)# -Inf  Inf
zd[,ii==0]

(storage.mode(print(1:1 / 0:0)))# Inf "double"
(storage.mode(print(1:1 / 1:1)))# 1 "double"
(storage.mode(print(1:1 + 1:1)))# 2 "integer"
(storage.mode(print(2:2 * 2:2)))# 4 "integer"
}
\keyword{programming}
\keyword{math}
