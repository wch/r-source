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
