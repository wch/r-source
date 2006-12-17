\name{match.arg}
\title{Argument Verification Using Partial Matching}
\usage{
match.arg(arg, choices, several.ok = FALSE)
}
\alias{match.arg}
\arguments{
  \item{arg}{a character vector (of length one unless \code{several.ok}
    is \code{TRUE}) or \code{NULL}.}
  \item{choices}{a character vector of candidate values}
  \item{several.ok}{logical specifying if \code{arg} should be allowed
    to have more than one element.}
}
\description{
  \code{match.arg} matches \code{arg} against a table of candidate
  values as specified by \code{choices}, where \code{NULL} means to take
  the first one.
}
\details{
  In the one-argument form \code{match.arg(arg)}, the choices are
  obtained from a default setting for the formal argument \code{arg} of
  the function from which \code{match.arg} was called.

  Matching is done using \code{\link{pmatch}}, so \code{arg} may be
  abbreviated.
}
\value{
  The unabbreviated version of the unique partial match if there is one;
  otherwise, an error is signalled if \code{several.ok} is false, as per
  default.  When \code{several.ok} is true and there is more than one
  match, all unabbreviated versions of matches are returned.
}
\seealso{
  \code{\link{pmatch}},
  \code{\link{match.fun}},
  \code{\link{match.call}}.
}
\examples{
require(stats)
## Extends the example for 'switch'
center <- function(x, type = c("mean", "median", "trimmed")) {
  type <- match.arg(type)
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
x <- rcauchy(10)
center(x, "t")       # Works
center(x, "med")     # Works
try(center(x, "m"))  # Error
stopifnot(identical(center(x),       center(x, "mean")),
          identical(center(x, NULL), center(x, "mean")) )

## Allowing more than one match:
% '3 of 4' -> warning when match.arg() is not coded well enough:
match.arg(c("gauss", "rect", "ep"),
          c("gaussian", "epanechnikov", "rectangular", "triangular"),
          several.ok = TRUE)
}
\keyword{programming}
