\name{Primitive}
\title{Call a ``Primitive'' Internal Function}
\usage{
--no real usage--
}
\alias{.Primitive}
\arguments{
\item{name}{name of the \R function.}
}
\description{
  \code{.Primitive} is not a real \R object, but rather the result of a
  syntactically pleasing print method for so called ``primitive''
  internal \R functions.

  The advantage of \code{.Primitive} over \code{\link{.Internal}}
  functions is the potential efficiency of argument passing.
}
\seealso{
\code{\link{.Internal}}.
}
\examples{
c
.Internal # this one *must* be primitive!
get("if") # just `if' or `print(if)' are not syntactically ok.
}
\keyword{interface}
