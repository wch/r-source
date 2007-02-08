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
  \item{\dots}{\code{expression}: \R objects, typically calls, symbols
    or constants.\cr
    \code{as.expression}: arguments to be passed to methods.}
  \item{x}{an arbitrary \R object.}
}
\details{
  \sQuote{Expression} here is not being used in its colloquial sense,
  that of mathematical expressions.  Those are calls (see
  \code{\link{call}}) in \R, and an \R expression vector is a list of
  calls, symbols etc, typically as returned by \code{\link{parse}}.

  As an object of mode \code{"expression"} is a list, it can be
  subsetted by both \code{[} and by \code{[[}, the latter extracting
  individual calls etc.
}
\value{
  \code{expression} returns a vector of type \code{"expression"}
  containing its arguments (unevaluated).

  \code{is.expression} returns \code{TRUE} if \code{expr} is an
  expression object and \code{FALSE} otherwise.

  \code{as.expression} attempts to coerce its argument into an
  expression object.  It is generic, and only the default method is
  described here.  \code{NULL}, calls, symbols (see
  \code{\link{as.symbol}}) and pairlists are returned as the element of
  a length-one expression vector.  Vectors (including lists) are placed
  element-by-element into an expression vector.  Other types are not
  currently supported.
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
  for plotting mathematical expressions.
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
