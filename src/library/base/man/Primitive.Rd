\name{Primitive}
\alias{.Primitive}
\title{Call a ``Primitive'' Internal Function}
\description{
  \code{.Primitive} returns an entry point to a \dQuote{primitive}
  (internally implemented) function.

  The advantage of \code{.Primitive} over \code{\link{.Internal}}
  functions is the potential efficiency of argument passing.
}
\usage{
.Primitive(name)
}
\arguments{
  \item{name}{name of the \R function.}
}
\seealso{
  \code{\link{.Internal}}.
}
\examples{
mysqrt <- .Primitive("sqrt")
c
.Internal # this one *must* be primitive!
get("if") # just 'if' or 'print(if)' are not syntactically ok.
}
\keyword{interface}
