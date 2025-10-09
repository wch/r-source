% File src/library/base/man/pretty.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{pretty}
\title{Pretty Breakpoints}
\alias{pretty}
\alias{.pretty}
\alias{pretty.default}
\usage{
pretty(x, \dots)

\method{pretty}{default}(x, n = 5, min.n = n \%/\% 3,  shrink.sml = 0.75,
       high.u.bias = 1.5, u5.bias = .5 + 1.5*high.u.bias,
       eps.correct = 0, f.min = 2^-20, \dots)

.pretty(x, n = 5L, min.n = n \%/\% 3,  shrink.sml = 0.75,
       high.u.bias = 1.5, u5.bias = .5 + 1.5*high.u.bias,
       eps.correct = 0L, f.min = 2^-20, bounds = TRUE)
}
\arguments{
  \item{x}{an object coercible to numeric by \code{\link{as.numeric}}.}
  \item{n}{integer giving the \emph{desired} number of
    intervals.  Non-integer values are rounded down.}
  \item{min.n}{nonnegative integer giving the \emph{minimal} number of
    intervals.  If \code{min.n == 0}, \code{pretty(.)} may return a
    single value.}
  \item{shrink.sml}{positive number, a factor (smaller than one)
    by which a default scale is shrunk in the case when
    \code{range(x)} is very small (usually 0).}
  \item{high.u.bias}{non-negative numeric, typically \eqn{> 1}.
    The interval unit is determined as \{1,2,5,10\} times \code{b}, a
    power of 10.  Larger \code{high.u.bias} values favor larger units.}
  \item{u5.bias}{non-negative numeric
    multiplier favoring factor 5 over 2.  Default and \sQuote{optimal}:
    \code{u5.bias = .5 + 1.5*high.u.bias}.}
  \item{eps.correct}{integer code, one of \{0,1,2\}.  If non-0, an
    \emph{epsilon correction} is made at the boundaries such that
    the result boundaries will be outside \code{range(x)}; in the
    \emph{small} case, this correction happens only if \code{eps.correct >= 2},
    allowing \code{eps.correct = 2} to reproduce how \R's graphics engine
    computes axis tick locations, see \sQuote{Details}.}
  \item{f.min}{positive factor multiplied by \code{\link{.Machine}$double.xmin}
    to get the smallest \dQuote{acceptable} \code{cell} \eqn{c_m} which
    determines the \code{unit} of the algorithm.  Smaller \code{cell}
    values are set to \eqn{c_n} signalling a \code{\link{warning}} about
    being \dQuote{corrected}.
    New from \R 4.2.0,: previously \code{f.min = 20} was
    hardcoded in the algorithm.}
  \item{bounds}{a \code{\link{logical}} indicating  if the resulting vector
    should \emph{cover} the full \code{range(x)}, i.e., strictly include
    the bounds of \code{x}.  New from \R 4.2.0, allowing \code{bounds=FALSE}
    to reproduce how \R's graphics engine computes axis extents and tick
    locations, see \sQuote{Details}; also, for \code{.pretty()}, it determines
    which parts of the C level computations should be returned.}
  \item{\dots}{further arguments for methods; unused in default method.}
}
\description{
  Compute a  sequence of about \code{n+1} equally spaced \sQuote{round}
  values which cover the range of the values in \code{x}.
  The values are chosen so that they are 1, 2 or 5 times a power of 10.
}
\details{
  \code{pretty} ignores non-finite values in \code{x}.

  Let \code{d <- max(x) - min(x)} \eqn{\ge 0}.
  If \code{d} is not (very close) to 0, we let \code{c <- d/n},
  otherwise more or less \code{c <- max(abs(range(x)))*shrink.sml / min.n}.
  Then, the \emph{10 base} \code{b} is
  \eqn{10^{\lfloor{\log_{10}(c)}\rfloor}}{10^(floor(log10(c)))} such
  that \eqn{b \le c < 10b}.

  Now determine the basic \emph{unit} \eqn{u} as one of
  \eqn{\{1,2,5,10\} b}{{1,2,5,10} b}, depending on
  \eqn{c/b \in [1,10)}{c/b in [1,10)}
  and the two \sQuote{\emph{bias}} coefficients, \eqn{h
  =}\code{high.u.bias} and \eqn{f =}\code{u5.bias}.

  \dots\dots\dots


  \R's graphics engine \code{GEPretty()} C function calls % ../../../main/engine.c
  \code{R_pretty()} and is used in both packages \pkg{graphics} and
  \pkg{grid} (and hence \CRANpkg{lattice}, \CRANpkg{ggplot2}, etc.) for
  axis range and tick computations.  For these, partly for back
  compatibility reasons, the optional arguments are set, corresponding to
\preformatted{
  (min.n = 1, shrink.sml = 0.25, high.u.bias = 0.8, u5.bias = 1.7,
   f.min = 1.125, eps.correct = 2, bounds = FALSE)}
}
\value{\code{pretty()} returns an numeric vector of \emph{approximately}
  \code{n} increasing numbers which are \dQuote{pretty} in decimal notation.
  (in extreme range cases, the numbers can no longer be \dQuote{pretty}
  given the other constraints; e.g., for \code{pretty(..)} %  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< FIXME
  For ease of investigating the underlying C \code{R_pretty()} % ../../../appl/pretty.c
  function, \code{.pretty()} returns a named \code{\link{list}}.  By
  default, when \code{bounds=TRUE}, the entries are \code{l}, \code{u},
  and \code{n}, whereas for \code{bounds=FALSE}, they are
  \code{ns}, \code{nu}, \code{n}, and (a \dQuote{pretty}) \code{unit}
  where the \code{n*}'s are integer valued (but only \code{n} is of class
  \code{\link{integer}}).  Programmers may use this to create pretty
  sequence (iterator) objects.
}
\seealso{
  \code{\link{axTicks}} for the computation of pretty axis tick
  locations in plots, particularly on the log scale.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
pretty(1:15)                    # 0  2  4  6  8 10 12 14 16
pretty(1:15, high.u.bias = 2)   # 0  5 10 15
pretty(1:15, n = 4)             # 0  5 10 15
pretty(1:15 * 2)                # 0  5 10 15 20 25 30
pretty(1:20)                    # 0  5 10 15 20
pretty(1:20, n = 2)             # 0 10 20
pretty(1:20, n = 10)            # 0  2  4 ... 20

for(k in 5:11) {
  cat("k=", k, ": "); print(diff(range(pretty(100 + c(0, pi*10^-k)))))}

##-- more bizarre, when  min(x) == max(x):
pretty(pi)

add.names <- function(v) { names(v) <- paste(v); v}
utils::str(lapply(add.names(-10:20), pretty))
## min.n = 0  returns a length-1 vector "if pretty":
utils::str(lapply(add.names(0:20),  pretty, min.n = 0))
sapply(    add.names(0:20),   pretty, min.n = 4)

pretty(1.234e100)
pretty(1001.1001)
pretty(1001.1001, shrink.sml = 0.2)
for(k in -7:3)
  cat("shrink=", formatC(2^k, width = 9),":",
      formatC(pretty(1001.1001, shrink.sml = 2^k), width = 6),"\n")
}
\keyword{dplot}
