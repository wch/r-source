\name{any}
\title{Are Some Values True?}
\usage{
any(\dots, na.rm = FALSE)
}
\alias{any}
\description{
  Given a set of logical vectors, are any of the values true?
}
\arguments{
  \item{\dots}{one or more logical vectors.  See also Details.}
  \item{na.rm}{logical. If true \code{NA} values are removed before
    the result is computed.}
}
\value{
  Given a sequence of logical arguments, a logical value indicating
  whether or not any of the elements of \code{x} are \code{TRUE}.

  The value returned is \code{TRUE} if any the values in \code{x}
  are \code{TRUE}, and \code{FALSE} if all the values in \code{x}
  are \code{FALSE}.

  If \code{x} consists of a mix of \code{FALSE} and \code{NA} values,
  the value is \code{NA}.
}
\details{
  This is a generic function: methods can be defined for it
  directly or via the \code{\link{Summary}} group generic.

  Integer, numeric and complex arguments are allowed and coerced to
  logical (with non-zero values being true).
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
range(x <- sort(round(rnorm(10) - 1.2,1)))
if(any(x < 0)) cat("x contains negative values\n")
}
\keyword{logic}
