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
%
\usage{
all.equal(target, current, \dots)

all.equal.numeric(target, current,
                  tolerance= .Machine$double.eps ^ 0.5, scale=NULL)
}
\arguments{
 \item{target}{\R object.}
 \item{current}{other \R object, to be compared with \code{target}.}

 \item{\dots}{Further arguments for different methods, notably the
     following two, for numerical comparison:}

 \item{tolerance}{numeric \eqn{\ge}{>=} 0.  Differences smaller than
     \code{tolerance} are not considered.}
 \item{scale}{numeric scalar > 0 (or \code{NULL});
     if specified, compare the average \emph{absolute} difference with
     \code{scale * tolerance}, see below.
}
\description{
    \code{all.equal(x,y)} is a utility to compare \R objects \code{x}
    and \code{y} testing ``near equality''.  If they are different,
    comparison is still made to some extent, and a report of the
    differences is returned.
}
\details{
    There are several methods available, most of which are dispatched by
    the default method, see \code{\link{methods}("all.equal")}.
    \code{all.equal.list} and \code{all.equal.language} provide
    comparison of recursive objects.

    Numerical comparison is done using averge (\code{\link{mean}})
    \emph{relative} error, unless \code{scale} is used or when the
    absolute difference is less than \code{tolerance}.
    For complex arguments, \code{\link{Mod}} of difference is used.

    \code{attr.all.equal} is used for comparing
    \code{\link{attributes}}, returning \code{NULL} or \code{character}.
}
\value{
    Either \code{TRUE} or a vector of \code{\link{mode}}
    \code{"character"} describing the differences between \code{target}
    and \code{current}.

  Numerical differences are reported by relative error
}
\seealso{\code{\link{==}}, and \code{\link{all}} for exact equality testing.}
\examples{
all.equal(pi, 355/113) # not precise enough (default tol) > relative error

all.equal(gamma(2:14),   cumprod(1:13)) # TRUE, but
all      (gamma(2:14) == cumprod(1:13)) # FALSE, since not exactly
all.equal(gamma(2:14),   cumprod(1:13), tol=0) # to see difference

all.equal(options(), .Options)
.Options $ myopt <- TRUE
all.equal(options(), as.list(.Options))
rm(.Options)
}
\keyword{utilities}
\keyword{logic}
\keyword{arith}


