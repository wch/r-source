\name{is.object}
\alias{is.object}
\title{Is an Object ``internally classed''?}
\usage{
is.object(x)
}
\description{
    A function rather for internal use.  It returns \code{TRUE} if the
    object \code{x} has the \R internal \code{OBJECT} attribute set, and
    \code{FALSE} otherwise.
}
\arguments{
  \item{x}{object to be tested.}
}
\details{
  \code{is.object} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\seealso{\code{\link[base]{class}}, and \code{\link{methods}}.}

\examples{
is.object(1) # FALSE
is.object(as.factor(1:3)) # TRUE
}
\keyword{methods}
\keyword{classes}
