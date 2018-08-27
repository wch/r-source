% File src/library/base/man/all.equal.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{all.equal}
\title{Test if Two Objects are (Nearly) Equal}
\alias{all.equal}
\alias{all.equal.default}
\alias{all.equal.numeric}
\alias{all.equal.character}
\alias{all.equal.environment}
\alias{all.equal.envRefClass}
\alias{all.equal.factor}
\alias{all.equal.formula}
\alias{all.equal.list}
\alias{all.equal.language}
\alias{all.equal.POSIXt}
\alias{all.equal.raw}
\alias{attr.all.equal}
\concept{numerical equality}
\concept{approximately equal}
\concept{equality testing}
\usage{
all.equal(target, current, \dots)

\method{all.equal}{numeric}(target, current,
          tolerance = sqrt(.Machine$double.eps), scale = NULL,
          countEQ = FALSE,
          formatFUN = function(err, what) format(err),
          \dots, check.attributes = TRUE)

\method{all.equal}{list}(target, current, \dots,
          check.attributes = TRUE, use.names = TRUE)

\method{all.equal}{environment}(target, current, all.names=TRUE, \dots)

\method{all.equal}{POSIXt}(target, current, \dots, tolerance = 1e-3, scale)


attr.all.equal(target, current, \dots,
               check.attributes = TRUE, check.names = TRUE)
}
\arguments{
  \item{target}{\R object.}
  \item{current}{other \R object, to be compared with \code{target}.}
  \item{\dots}{further arguments for different methods, notably the
    following two, for numerical comparison:}
  \item{tolerance}{numeric \eqn{\ge} 0.  Differences smaller than
    \code{tolerance} are not reported.  The default value is close to
    \code{1.5e-8}.}
  \item{scale}{numeric scalar > 0 (or \code{NULL}). See \sQuote{Details}.}
  \item{countEQ}{logical indicating if the \code{target == current}
    cases should be counted when computing the mean (absolute or
    relative) differences.  The default, \code{FALSE} may seem
    misleading in cases where \code{target} and \code{current} only
    differ in a few places; see the extensive example.}
  \item{formatFUN}{a \code{\link{function}} of two arguments,
    \code{err}, the relative, absolute or scaled error, and
    \code{what}, a character string indicating the \emph{kind} of error;
    maybe used, e.g., to format relative and absolute errors differently.}
  \item{check.attributes}{logical indicating if the
    \code{\link{attributes}} of \code{target} and \code{current}
    (other than the names) should be compared.}
  \item{use.names}{logical indicating if \code{\link{list}} comparison
    should report differing components by name (if matching) instead of
    integer index.  Note that this comes after \code{\dots} and so must
    be specified by its full name.}
  \item{all.names}{logical passed to \code{\link{ls}} indicating if
    \dQuote{hidden} objects should also be considered in the environments.}
  \item{check.names}{logical indicating if the \code{\link{names}(.)}
    of \code{target} and \code{current} should be compared.}
}
\description{
    \code{all.equal(x, y)} is a utility to compare \R objects \code{x}
    and \code{y} testing \sQuote{near equality}.  If they are different,
    comparison is still made to some extent, and a report of the
    differences is returned.    Do not use \code{all.equal} directly in
    \code{if} expressions---either use \code{isTRUE(all.equal(....))} or
    \code{\link{identical}} if appropriate.
}
\details{
  \code{all.equal} is a generic function, dispatching methods on the
  \code{target} argument.  To see the available methods, use
  \code{\link{methods}("all.equal")}, but note that the default method
  also does some dispatching, e.g.\sspace{}using the raw method for logical
  targets.

  Remember that arguments which follow \code{\dots} must be specified by
  (unabbreviated) name.  It is inadvisable to pass unnamed arguments in
  \code{\dots} as these will match different arguments in different
  methods.

  Numerical comparisons for \code{scale = NULL} (the default) are
  typically on \emph{relative difference} scale unless the target values
  are close to zero:  First, the mean absolute difference of the two
  numerical vectors is computed.  If this is smaller than
  \code{tolerance} or not finite, absolute differences are used,
  otherwise relative differences scaled by the mean absolute
  \code{target} value.
  Note that these comparisons are computed only for those vector elements
  where \code{target} is not \code{\link{NA}} and differs from \code{current}.
  If \code{countEQ} is true, the equal and \code{NA} cases are
  \emph{counted} in determining \dQuote{sample} size.

  If \code{scale} is positive, absolute comparisons are made after
  scaling (dividing) by \code{scale}.

  For complex \code{target}, the modulus (\code{\link{Mod}}) of the
  difference is used: \code{all.equal.numeric} is called so arguments
  \code{tolerance} and \code{scale} are available.

  The \code{\link{list}} method compares components of
  \code{target} and \code{current} recursively, passing all other
  arguments, as long as both are \dQuote{list-like}, i.e., fulfill
  either \code{\link{is.vector}} or \code{\link{is.list}}.

  The \code{\link{environment}} method works via the \code{list} method,
  and is also used for reference classes (unless a specific
  \code{all.equal} method is defined).

  The methods for the date-time classes by default allow a tolerance of
  \code{tolerance = 0.001} seconds, and ignore \code{scale}.

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
all.equal(tan(d45), rep(1, 10), tolerance = 0)  # to see difference

## advanced: equality of environments
ae <- all.equal(as.environment("package:stats"),
                asNamespace("stats"))
stopifnot(is.character(ae), length(ae) > 10,
          ## were incorrectly "considered equal" in R <= 3.1.1
          all.equal(asNamespace("stats"), asNamespace("stats")))

## A situation where  'countEQ = TRUE' makes sense:
x1 <- x2 <- (1:100)/10;  x2[2] <- 1.1*x1[2]
## 99 out of 100 pairs (x1[i], x2[i]) are equal:
plot(x1,x2, main = "all.equal.numeric() -- not counting equal parts")
all.equal(x1,x2) ## "Mean relative difference: 0.1"
mtext(paste("all.equal(x1,x2) :", all.equal(x1,x2)), line= -2)
##' extract the 'Mean relative difference' as number:
all.eqNum <- function(...) as.numeric(sub(".*:", '', all.equal(...)))
set.seed(17)
## When x2 is jittered, typically all pairs (x1[i],x2[i]) do differ:
summary(r <- replicate(100, all.eqNum(x1, x2*(1+rnorm(x1)*1e-7))))
mtext(paste("mean(all.equal(x1, x2*(1 + eps_k))) {100 x} Mean rel.diff.=",
            signif(mean(r), 3)), line = -4, adj=0)
## With argument  countEQ=TRUE, get "the same" (w/o need for jittering):
mtext(paste("all.equal(x1,x2, countEQ=TRUE) :",
          signif(all.eqNum(x1,x2, countEQ=TRUE), 3)), line= -6, col=2)
}
\keyword{programming}% is.*
\keyword{utilities}
\keyword{logic}
\keyword{arith}
