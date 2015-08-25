% File src/library/base/man/is.object.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{is.object}
\alias{is.object}
\title{Is an Object \sQuote{internally classed}?}
\usage{
is.object(x)
}
\description{
    A function rather for internal use.  It returns \code{TRUE} if the
    object \code{x} has the \R internal \code{OBJECT} bit set, and
    \code{FALSE} otherwise.  The \code{OBJECT} bit is set when a
    \code{"class"} attribute is added and removed when that attribute is
    removed, so this is a very efficient way to check if an object has a
    class attribute.  (S4 objects always should.)
}
\arguments{
  \item{x}{object to be tested.}
}
\note{
  This is a \link{primitive} function.
}
\seealso{
  \code{\link{class}}, and \code{\link{methods}}.

  \code{\link{isS4}}.
}

\examples{
is.object(1) # FALSE
is.object(as.factor(1:3)) # TRUE
}
\keyword{methods}
\keyword{classes}
