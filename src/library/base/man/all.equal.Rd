\name{all.equal}
\title{Test if Two Objects are (Nearly) Equal}
\alias{all.equal}
\alias{all.equal.default}
\alias{all.equal.numeric}
\alias{all.equal.character}
\alias{all.equal.factor}
\alias{all.equal.formula}
\alias{all.equal.list}
\alias{all.equal.language}
\alias{attr.all.equal}
\usage{
all.equal(target, current, \dots)

\method{all.equal}{numeric}(target, current,
          tolerance= .Machine$double.eps ^ 0.5, scale=NULL, \dots)

attr.all.equal(target, current, \dots)
}
\arguments{
  \item{target}{\R object.}
  \item{current}{other \R object, to be compared with \code{target}.}
  \item{\dots}{Further arguments for different methods, notably the
    following two, for numerical comparison:}
  \item{tolerance}{numeric \eqn{\ge}{>=} 0.  Differences smaller than
    \code{tolerance} are not considered.}
  \item{scale}{numeric scalar > 0 (or \code{NULL}). See Details.}
}
\description{
    \code{all.equal(x,y)} is a utility to compare \R objects \code{x}
    and \code{y} testing \dQuote{near equality}.  If they are different,
    comparison is still made to some extent, and a report of the
    differences is returned.  Don't use \code{all.equal} directly in
    \code{if} expressions---either use \code{\link{identical}} or combine
    the two, as shown in the documentation for \code{identical}.
}
\details{
    There are several methods available, most of which are dispatched by
    the default method, see \code{\link{methods}("all.equal")}.
    \code{all.equal.list} and \code{all.equal.language} provide
    comparison of recursive objects.

    Numerical comparisons for \code{scale = NULL} (the default) are done
    by first computing the mean absolute difference of the two numerical
    vectors. If this is smaller than \code{tolerance} or not finite,
    absolute differences are used, otherwise relative differences scaled
    by the mean absolute difference.

    If \code{scale} is positive, absolute comparisons are after scaling
    (dividing) by \code{scale}.

    For complex arguments, the modulus \code{\link{Mod}} of the difference 
    is used: \code{all.numeric.numeric} is called so arguments
    \code{tolerance} and \code{scale} are available.

    \code{attr.all.equal} is used for comparing
    \code{\link{attributes}}, returning \code{NULL} or \code{character}.
}
\value{
    Either \code{TRUE} or a vector of \code{\link{mode}}
    \code{"character"} describing the differences between \code{target}
    and \code{current}.

  Numerical differences are reported by relative error
}
\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer (for \code{=}).
}
\seealso{\code{\link{identical}}, \code{\link{==}}, and \code{\link{all}} 
for exact equality testing.}
\examples{
all.equal(pi, 355/113) # not precise enough (default tol) > relative error

d45 <- pi*(1/4 + 1:10)
stopifnot(
all.equal(tan(d45), rep(1,10))) # TRUE, but
all      (tan(d45) == rep(1,10)) # FALSE, since not exactly
all.equal(tan(d45), rep(1,10), tol=0) # to see difference

all.equal(options(), .Options)
all.equal(options(), as.list(.Options))# TRUE
.Options $ myopt <- TRUE
all.equal(options(), as.list(.Options))
rm(.Options)
}
\keyword{utilities}
\keyword{logic}
\keyword{arith}


