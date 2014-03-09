% File src/library/base/man/Primitive.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{Primitive}
\alias{.Primitive}
\alias{primitive}
\title{Look Up a Primitive Function}
\description{
  \code{.Primitive} looks up by name a \sQuote{primitive}
  (internally implemented) function.
}
\usage{
.Primitive(name)
}
\arguments{
  \item{name}{name of the \R function.}
}
\details{
  The advantage of \code{.Primitive} over \code{\link{.Internal}}
  functions is the potential efficiency of argument passing, and that
  positional matching can be used where desirable, e.g. in
  \code{\link{switch}}. For more details, see the \sQuote{R Internals
    Manual}.

  All primitive functions are in the base namespace.

  This function is almost never used: \code{`name`} or, more carefully,
  \code{\link{get}(name, envir = baseenv())} work equally well and do
  not depend on knowing which functions are primitive (which does change
  as \R evolves).
}
\seealso{
  \code{\link{.Internal}}.
}
\examples{
mysqrt <- .Primitive("sqrt")
c
.Internal # this one *must* be primitive!
`if` # need backticks
}
\keyword{interface}
