\name{all}
\title{Are All Values True?}
\usage{
all(\dots, na.rm = FALSE)
}
\alias{all}
\description{
  Given a set of logical vectors, are all of the values true?
}
\arguments{
  \item{\dots}{one or more logical vectors.  Other objects are coerced in
    a similar way as \code{as.logical.default}.}
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
  Given a sequence of logical arguments, a logical value indicating
  whether or not all of the elements of \code{x} are \code{TRUE}.

  The value returned is \code{TRUE} if all of the values in \code{x}
  are \code{TRUE}, and \code{FALSE} if any of the values in \code{x}
  are \code{FALSE}.

  If \code{na.rm = FALSE} and \code{x} consists of a mix of \code{TRUE}
  and \code{NA} values, the value is \code{NA}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{any}}, the \dQuote{complement} of \code{all}, and
  \code{\link{stopifnot}(*)} which is an \code{all(*)}
  \dQuote{insurance}.
}
\examples{
range(x <- sort(round(rnorm(10) - 1.2, 1)))
if(all(x < 0)) cat("all x values are negative\n")
}
\keyword{logic}
