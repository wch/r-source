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
  \item{\dots}{one or more logical vectors.  Other objects are coerced in
    a similar way as \code{as.logical.default}.}
  \item{na.rm}{logical.  If true \code{NA} values are removed before
    the result is computed.}
}
\details{
  This is a generic function: methods can be defined for it
  directly or via the \code{\link{Summary}} group generic.
}
\value{
  Given a sequence of logical arguments, a logical value indicating
  whether or not any of the elements of \code{x} are \code{TRUE}.

  The value returned is \code{TRUE} if any the values in \code{x}
  are \code{TRUE}, and \code{FALSE} if all the values in \code{x}
  are \code{FALSE}.

  If \code{na.rm = FALSE} and \code{x} consists of a mix of \code{FALSE}
  and \code{NA} values, the value is \code{NA}.
}
\note{
  Prior to \R 2.1.0, only \code{NULL} and logical, integer, numeric
  and complex vectors were accepted.
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
