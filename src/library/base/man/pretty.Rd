\name{pretty}
\title{Pretty Breakpoints}
\usage{
pretty(x, n = 5, min.n = n \%/\% 3,  shrink.sml = 0.75,
       high.u.bias = 1.5, u5.bias = .5 + 1.5*high.u.bias,
       eps.correct = 0)
}
\alias{pretty}
\arguments{
\item{x}{numeric vector}
\item{n}{integer giving the \emph{desired} number of
  intervals.  Non-integer values are rounded down.}
\item{min.n}{nonnegative integer giving the \emph{minimal} number of
    intervals.  If \code{min.n == 0}, \code{pretty(.)} may return a
    single value.}
\item{shrink.sml}{positive numeric
  by a which a default scale is shrunk in the case when
  \code{range(x)} is \dQuote{very small} (usually 0).}
\item{high.u.bias}{non-negative numeric, typically \eqn{> 1}.
    The interval unit is determined as \{1,2,5,10\} times \code{b}, a
    power of 10.  Larger \code{high.u.bias} values favor larger units.
}
\item{u5.bias}{non-negative numeric
    multiplier favoring factor 5 over 2.  Default and \dQuote{optimal}:
    \code{u5.bias = .5 + 1.5*high.u.bias}.}
\item{eps.correct}{integer code, one of \{0,1,2\}. If non-0, an
    \dQuote{\emph{epsilon correction}} is made at the boundaries such that
    the result boundaries will be outside \code{range(x)}; in the
    \emph{small} case, the correction is only done if \code{eps.correct >=2}.}
}
\description{
    Compute a  sequence of about \code{n+1} equally spaced nice values which
    cover the range of the values in \code{x}.
    The values are chosen so that they are 1, 2 or 5 times a power of 10.
}
\details{
    Let \code{d <- max(x) - min(x)} \eqn{\ge 0}{>= 0}.
    If \code{d} is not (very close) to 0, we let \code{c <- d/n},
    otherwise more or less \code{c <- max(abs(range(x)))*shrink.sml / min.n}.
    Then, the \emph{10 base} \code{b} is
    \eqn{10^{\lfloor{\log_{10}(c)}\rfloor}}{10^(floor(log10(c)))} such
    that \eqn{b \le c < 10b}.

    Now determine the basic \emph{unit} \eqn{u} as one of
    \eqn{\{1,2,5,10\} b}, depending on \eqn{c/b \in [1,10)} and the two
    \dQuote{\emph{bias}} coefficients, \eqn{h =}\code{high.u.bias} and
    \eqn{f =}\code{u5.bias}.

    \dots\dots\dots
%%-- fixme: give even more details
%%-- fixme: give even more details
}
\seealso{
  \code{\link{axTicks}} for the computation of pretty axis tick
  locations in plots, particularly on the log scale.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
pretty(1:15)     # 0  2  4  6  8 10 12 14 16
pretty(1:15, h=2)# 0  5 10 15
pretty(1:15, n=4)# 0  5 10 15
pretty(1:15 * 2) # 0  5 10 15 20 25 30
pretty(1:20)      # 0  5 10 15 20
pretty(1:20, n=2) # 0 10 20
pretty(1:20, n=10)# 0  2  4 ... 20

for(k in 5:11) {
  cat("k=",k,": "); print(diff(range(pretty(100 + c(0, pi*10^-k)))))}

##-- more bizarre, when  min(x) == max(x):
pretty(pi)

add.names <- function(v) { names(v) <- paste(v); v}
str(lapply(add.names(-10:20), pretty))
str(lapply(add.names(0:20),   pretty, min = 0))
sapply(    add.names(0:20),   pretty, min = 4)

pretty(1.234e100)
pretty(1001.1001)
pretty(1001.1001, shrink = .2)
for(k in -7:3)
  cat("shrink=",formatC(2^k,wid=9),":",
      formatC(pretty(1001.1001, shrink = 2^k), wid=6),"\n")
}
\keyword{dplot}
