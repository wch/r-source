\name{Paren}
\alias{Paren}
\alias{(}
\alias{{}
\title{Parentheses and Braces}
\usage{
( \dots )

{ \dots }
}
\description{
  Open parenthesis, \code{(}, and open brace, \code{\{}, are
  \code{\link{.Primitive}} functions in \R.

  Effectively, \code{(} is semantically equivalent to the identity
  \code{function(x) x}, whereas \code{\{} is slightly more interesting,
  see examples.
}
\seealso{
  \code{\link{if}}, \code{\link{return}}, etc for other objects used in
  the \R language itself.

  \code{\link{Syntax}} for operator precedence.
}
\examples{
f <- get("(")
e <- expression(3 + 2 * 4)
f(e) == e           # TRUE

do <- get("\{")
do(x <- 3, y <- 2*x-3, 6-x-y); x; y
}
\keyword{programming}
