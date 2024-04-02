% File src/library/base/man/all.equal.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
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
\alias{all.equal.function}
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

\method{all.equal}{default}(target, current, \dots, check.class = TRUE)

\method{all.equal}{numeric}(target, current,
          tolerance = sqrt(.Machine$double.eps), scale = NULL,
          countEQ = FALSE,
          formatFUN = function(err, what) format(err),
          \dots, check.attributes = TRUE, check.class = TRUE, giveErr = FALSE)

\method{all.equal}{list}(target, current, \dots,
          check.attributes = TRUE, use.names = TRUE)

\method{all.equal}{environment}(target, current, all.names = TRUE,
          evaluate = TRUE, \dots)

\method{all.equal}{function}(target, current, check.environment=TRUE, \dots)

\method{all.equal}{POSIXt}(target, current, \dots, tolerance = 1e-3, scale,
          check.tzone = TRUE)


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
  \item{scale}{\code{NULL} or numeric > 0, typically of length 1 or
    \code{length(target)}.  See \sQuote{Details}.}
  \item{countEQ}{logical indicating if the \code{target == current}
    cases should be counted when computing the mean (absolute or
    relative) differences.  The default, \code{FALSE} may seem
    misleading in cases where \code{target} and \code{current} only
    differ in a few places; see the extensive example.}
  \item{formatFUN}{a \code{\link{function}} of two arguments,
    \code{err}, the relative, absolute or scaled error, and
    \code{what}, a character string indicating the \emph{kind} of error;
    may be used, e.g., to format relative and absolute errors differently.}
  \item{check.attributes}{logical indicating if the
    \code{\link{attributes}} of \code{target} and \code{current}
    (other than the names) should be compared.}
  \item{check.class}{logical indicating if the \code{\link{data.class}()}
    of \code{target} and \code{current} should be compared.}
  \item{giveErr}{\code{\link{logical}} indicating if the result should
    contain the numerical error as an \code{"err"} attribute.}
  \item{use.names}{logical indicating if \code{\link{list}} comparison
    should report differing components by name (if matching) instead of
    integer index.  Note that this comes after \code{\dots} and so must
    be specified by its full name.}
  \item{all.names}{logical passed to \code{\link{ls}} indicating if
    \dQuote{hidden} objects should also be considered in the environments.}
  \item{evaluate}{for the \code{environment} method: \code{\link{logical}} indicating if
    \dQuote{promises should be forced}, i.e., typically formal function arguments
    be evaluated for comparison.  If false, only the \code{\link{names}} of
    the objects in the two environments are checked for equality.}
  \item{check.environment}{logical requiring that the
    \code{\link{environment}()}s of functions should be compared, too.
    You may need to set \code{check.environment=FALSE} in unexpected
    cases, such as when comparing two \code{\link{nls}()} fits.}
  \item{check.tzone}{logical indicating if the \code{"tzone"} attributes
    of \code{target} and \code{current} should be compared.}
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
  typically on a \emph{relative difference} scale unless the
  \code{target} values are close to zero or infinite. Specifically,
  the scale is computed as the mean absolute value of \code{target}.
  If this scale is finite and exceeds \code{tolerance}, differences
  are expressed relative to it; otherwise, absolute differences are used.
  Note that this scale and all further steps are computed only for those
  vector elements
  where \code{target} is not \code{\link{NA}} and differs from \code{current}.
  If \code{countEQ} is true, the equal and \code{NA} cases are
  \emph{counted} in determining the \dQuote{sample} size.

  If \code{scale} is numeric (and positive), absolute comparisons are
  made after scaling (dividing) by \code{scale}.  Note that if all of
  scale is close to 1 (specifically, within 1e-7), the difference is still
  reported as being on an absolute scale.

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

  The method for date-time objects uses \code{all.equal.numeric} to
  compare times (in \code{"\link{POSIXct}"} representation) with a
  default \code{tolerance} of 0.001 seconds, ignoring \code{scale}.
  A time zone mismatch between \code{target} and \code{current} is
  reported unless \code{check.tzone = FALSE}.

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

quarts <- 1/4 + 1:10 # exact
d45 <- pi*quarts ; one <- rep(1, 10)
tan(d45) == one  # mostly FALSE, as typically exact; embarrassingly,
tanpi(quarts) == one # (is always FALSE (Fedora 34; gcc 11.2.1))
stopifnot(all.equal(
          tan(d45), one)) # TRUE, but not if we are picky:
all.equal(tan(d45), one, tolerance = 0)  # to see difference
all.equal(tan(d45), one, tolerance = 0, scale = 1)# "absolute diff.."
all.equal(tan(d45), one, tolerance = 0, scale = 1+(-2:2)/1e9) # "absolute"
all.equal(tan(d45), one, tolerance = 0, scale = 1+(-2:2)/1e6) # "scaled"

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

## Using giveErr=TRUE :
x1. <- x1 * (1+ 1e-9*rnorm(x1))
str(all.equal(x1, x1., giveErr=TRUE))
## logi TRUE
## - attr(*,  "err")= num 8.66e-10
## - attr(*, "what")= chr "relative"

## Used with stopifnot(), still *showing* diff:
all.equalShow <- function (...) {
   r <- all.equal(..., giveErr=TRUE)
   cat(attr(r,"what"), "err:", attr(r,"err"), "\n")
   c(r) # can drop attributes, as not used anymore
}
# checks, showing error in any case:
stopifnot(all.equalShow(x1, x1.)) # -> relative err: 8.66002e-10
tryCatch(error=identity, stopifnot(all.equalShow(x1, 2*x1))) -> eAe
stopifnot(inherits(eAe, "error"))
# stopifnot(all.equal....()) giving smart msg:
cat(conditionMessage(eAe), "\n")
\dontshow{
if (Sys.getenv("LANGUAGE") == "en" || !capabilities("NLS"))
  stopifnot(grepl("are not equal", conditionMessage(eAe), fixed=TRUE))
}
two <- structure(2, foo = 1, class = "bar")
all.equal(two^20, 2^20) # lots of diff
all.equal(two^20, 2^20, check.attributes = FALSE)# "target is bar, current is numeric"
all.equal(two^20, 2^20, check.attributes = FALSE, check.class = FALSE) # TRUE

## comparison of date-time objects
now <- Sys.time()
stopifnot(
all.equal(now, now + 1e-4)  # TRUE (default tolerance = 0.001 seconds)
)
all.equal(now, now + 0.2)
all.equal(now, as.POSIXlt(now, "UTC"))
stopifnot(
all.equal(now, as.POSIXlt(now, "UTC"), check.tzone = FALSE)  # TRUE
)
}
\keyword{programming}% is.*
\keyword{utilities}
\keyword{logic}
\keyword{arith}
