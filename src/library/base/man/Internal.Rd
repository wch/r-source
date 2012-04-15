% File src/library/base/man/Internal.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{Internal}
\alias{.Internal}
\description{
\code{.Internal} performs a call to an internal code
which is built in to the \R interpreter.

Only true \R wizards should even consider using this function, and only
\R developers can add to the list of internal functions.
}
\title{Call an Internal Function}
\usage{
 .Internal(call)
}
\arguments{
  \item{call}{a call expression}
}
\seealso{
  \code{\link{.Primitive}}, \code{\link{.External}} (the nearest
  equivalent available to  users).
}
\keyword{interface}
