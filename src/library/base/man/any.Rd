\name{any}
\title{Are Some Values True?}
\usage{
any(\dots, na.rm = FALSE)
}
\alias{any}
\description{
  Given a set of logical vectors, is at least one of the values true?
}
\arguments{
  \item{\dots}{zero or more logical vectors.  Other objects are coerced in
    a similar way to \code{as.logical.default}.}
  \item{na.rm}{logical.  If true \code{NA} values are removed before
    the result is computed.}
}
\details{
  This is a generic function: methods can be defined for it
  directly or via the \code{\link[base:groupGeneric]{Summary}} group generic.
  For this to work properly, the arguments \code{\dots} should be
  unnamed, and dispatch is on the first argument.
}
\value{
  The value is a logical vector of length one.
  
  Let \code{x} denote the concatenation of all the logical vectors in
  \code{...} (after coercion), after removing \code{NA}s if requested by
  \code{na.rm = TRUE}.

  The value returned is \code{TRUE} if at least one of the values in
  \code{x} is \code{TRUE}, and \code{FALSE} if all of the values in
  \code{x} are \code{FALSE} (including if there are no values).  Otherwise
  the value is \code{NA} (which can only occur if \code{na.rm = FALSE}
  and \code{\dots} contains no \code{TRUE} values and at least one
  \code{NA} value).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{all}}, the \dQuote{complement} of \code{any}.
}
\examples{
range(x <- sort(round(stats::rnorm(10) - 1.2,1)))
if(any(x < 0)) cat("x contains negative values\n")
}
\keyword{logic}
