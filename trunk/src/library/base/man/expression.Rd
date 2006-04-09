\name{expression}
\alias{expression}
\alias{is.expression}
\alias{as.expression}
\alias{as.expression.default}
\title{Unevaluated Expressions}
\description{
  Creates or tests for objects of mode \code{"expression"}.
}
\usage{
expression(\dots)

is.expression(x)
as.expression(x, \dots)
}
\arguments{
  \item{\dots}{valid \R expressions.}
  \item{x}{an arbitrary \R object.}
}
\value{
  \code{expression} returns a vector of mode \code{"expression"}
  containing its arguments as unevaluated \dQuote{\link{call}s}.

  \code{is.expression} returns \code{TRUE} if \code{expr} is an
  expression object and \code{FALSE} otherwise.

  \code{as.expression} attempts to coerce its argument into an
  expression object.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{call}},
  \code{\link{eval}},
  \code{\link{function}}.
  Further,
  \code{\link{text}} and
  \code{\link{legend}}
  for plotting math expressions.
}
\examples{
length(ex1 <- expression(1+ 0:9))# 1
ex1
eval(ex1)# 1:10

length(ex3 <- expression(u,v, 1+ 0:9))# 3
mode(ex3 [3]) # expression
mode(ex3[[3]])# call
rm(ex3)
}
\keyword{programming}
\keyword{dplot}
