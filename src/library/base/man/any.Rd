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
  \item{\dots}{one or more logical vectors.}
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
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{\code{\link{all}}, the ``complement'' of \code{any}.
}
\examples{
range(x <- sort(round(rnorm(10) - 1.2,1)))
if(any(x < 0)) cat("x contains negative values\n")
}
\keyword{logic}
