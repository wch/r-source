% File src/library/base/man/Primitive.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
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
  positional matching can be used where desirable, e.g.\sspace{}in
  \code{\link{switch}}. For more details, see the \sQuote{R Internals}
  manual.

  All primitive functions are in the base namespace.

  You should not set \code{\link{attributes}} on primitive functions.
  This now signals a deprecation \code{\link{warning}} and will become illegal (via error), soon.

  This function is almost never used: \code{`name`} or, more carefully,
  \code{\link{get}(name, envir = baseenv())} work equally well and do
  not depend on knowing which functions are primitive (which does change
  as \R evolves).
}
\seealso{
  \code{\link{is.primitive}} showing that primitive functions come in
  two types (\code{\link{typeof}}),
  \code{\link{.Internal}}.
}
\examples{
mysqrt <- .Primitive("sqrt")
\donttest{ ## be careful, this changes base R's sqrt() {until restarting R} !
try(structure(mysqrt, comment = "primitive function")) # deprecation warning; soon an error
}
.Internal # this one *must* be primitive!
`if` # need backticks
}
\keyword{interface}
