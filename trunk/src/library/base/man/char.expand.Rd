\name{char.expand}
\alias{char.expand}
\title{Expand a String with Respect to a Target Table}
\usage{char.expand(input, target, nomatch = stop("no match"))}
\description{Seeks a unique match of its first argument among the
  elements of its second.  If successful, it returns this element;
  otherwise, it performs an action specified by the third argument.}
\arguments{
  \item{input}{a character string to be expanded.}
  \item{target}{a character vector with the values to be matched
    against.}
  \item{nomatch}{an \R expression to be evaluated in case expansion was
    not possible.}
}
\details{
  This function is particularly useful when abbreviations are allowed in
  function arguments, and need to be uniquely expanded with respect to a
  target table of possible values.
}
\seealso{
  \code{\link{charmatch}} and \code{\link{pmatch}} for performing
  partial string matching.
}
\examples{
locPars <- c("mean", "median", "mode")
char.expand("me", locPars, warning("Could not expand!"))
char.expand("mo", locPars)
}
\keyword{character}
