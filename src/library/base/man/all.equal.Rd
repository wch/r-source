% File src/library/base/man/all.equal.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

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
\alias{all.equal.POSIXct}
\alias{all.equal.raw}
\alias{attr.all.equal}
\concept{numerical equality}
\concept{approximately equal}
\concept{equality testing}
\usage{
all.equal(target, current, \dots)

\method{all.equal}{numeric}(target, current,
          tolerance = .Machine$double.eps ^ 0.5,
          scale = NULL, check.attributes = TRUE, \dots)

\method{all.equal}{list}(target, current,
          check.attributes = TRUE, \dots, use.names = TRUE)

attr.all.equal(target, current, \dots,
               check.attributes = TRUE, check.names = TRUE)
}
\arguments{
  \item{target}{\R object.}
  \item{current}{other \R object, to be compared with \code{target}.}
  \item{\dots}{Further arguments for different methods, notably the
    following two, for numerical comparison:}
  \item{tolerance}{numeric \eqn{\ge} 0.  Differences smaller than
    \code{tolerance} are not considered.}
  \item{scale}{numeric scalar > 0 (or \code{NULL}). See \sQuote{Details}.}
  \item{check.attributes}{logical indicating if the
    \code{\link{attributes}} of \code{target} and \code{current}
    (other than the names) should be compared.}
  \item{use.names}{logical indicating if \code{\link{list}} comparison
    should report differing components by name (if matching) instead of
    integer index.  Note that this comes after \code{\dots} and so must
    be specified by its full name.}
  \item{check.names}{logical indicating if the \code{\link{names}(.)}
    of \code{target} and \code{current} should be compared.}
}
\description{
    \code{all.equal(x, y)} is a utility to compare \R objects \code{x}
    and \code{y} testing \sQuote{near equality}.  If they are different,
    comparison is still made to some extent, and a report of the
    differences is returned.    Don't use \code{all.equal} directly in
    \code{if} expressions---either use \code{isTRUE(all.equal(....))} or
    \code{\link{identical}} if appropriate.
}
\details{
  \code{all.equal} is a generic function, dispatching methods on the
  \code{target} argument.  To see the available methods, use
  \code{\link{methods}("all.equal")}, but note that the default method
  also does some dispatching, e.g. using the raw method for logical
  targets.

  Numerical comparisons for \code{scale = NULL} (the default) are done
  by first computing the mean absolute difference of the two numerical
  vectors.  If this is smaller than \code{tolerance} or not finite,
  absolute differences are used, otherwise relative differences scaled
  by the mean absolute difference.

  If \code{scale} is positive, absolute comparisons are made after
  scaling (dividing) by \code{scale}.

  For complex \code{target}, the modulus (\code{\link{Mod}}) of the
  difference is used: \code{all.equal.numeric} is called so arguments
  \code{tolerance} and \code{scale} are available.

  The \code{\link{list}} method compares components of
  \code{target} and \code{current} recursively, passing all other
  arguments, as long as both are \dQuote{list-like}, i.e., fulfill
  either \code{\link{is.vector}} or \code{\link{is.list}}.

  The method for the date-time class \code{"\link{POSIXct}"} by default
  allows a tolerance of \code{tolerance = 0.001} seconds.

  \code{attr.all.equal} is used for comparing
  \code{\link{attributes}}, returning \code{NULL} or a
  \code{character} vector.
}
\value{
  Either \code{TRUE} (\code{NULL} for \code{attr.all.equal}) or a vector
  of \code{\link{mode}} \code{"character"} describing the differences
  between \code{target} and \code{current}.
}
\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer (for \code{=}).
}
\seealso{\code{\link{identical}}, \code{\link{isTRUE}}, \code{\link{==}}, and
  \code{\link{all}} for exact equality testing.
}
\examples{
all.equal(pi, 355/113)
# not precise enough (default tol) > relative error

d45 <- pi*(1/4 + 1:10)
stopifnot(
all.equal(tan(d45), rep(1, 10)))          # TRUE, but
all      (tan(d45) == rep(1, 10))         # FALSE, since not exactly
all.equal(tan(d45), rep(1, 10), tol = 0)  # to see difference
}
\keyword{programming}% is.*
\keyword{utilities}
\keyword{logic}
\keyword{arith}
