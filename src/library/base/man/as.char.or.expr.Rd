\name{as.char.or.expr}
\alias{as.char.or.expr}
\title{Coerce to Character or Expression}
\usage{as.char.or.expr(x)}
\description{
  Coerce expression and call objects to expressions, and all other
  objects to character.
}
\arguments{
  \item{x}{an arbitrary \R object.}
}
\details{
  This function is used for handling the \code{text} arguments to the
  text-drawing functions.  These arguments can be character strings or
  expressions (which are interpreted as mathematical expressions and
  typeset according to TeX-like rules, see \code{\link{plotmath}} for
  more details).  Users will typically have no need for it.
}
\examples{
as.char.or.expr("This is a string.")
as.char.or.expr(call("round", 10.5))
}
\keyword{dplot}
\keyword{utilities}
