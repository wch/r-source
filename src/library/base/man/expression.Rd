\name{expression}
\title{Unevaluated Expressions}
\usage{
expression(\dots)

is.expression(expr)
as.expression(expr)
}
\alias{expression}
\alias{is.expression}
\alias{as.expression}
\value{

\code{expression} returns a vector of mode \code{"expression"}
containing its arguments as unevaluated ``\link{call}s''.

\code{is.expression} returns \code{TRUE} if \code{expr} is
an expression object and \code{FALSE} otherwise.

\code{as.expression} attempts to coerce its argument
into an expression object.
}
\seealso{
  \code{\link{call}}, \code{\link{eval}}, \code{\link{function}}.
  Further, \code{\link{text}} and \code{\link{legend}} for plotting math
  expressions.
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
