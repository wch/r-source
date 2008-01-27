% File src/library/base/man/isS4.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{isS4}
\alias{isS4}
\alias{asS4}
\title{ Test for an S4 object }
\description{
  Tests whether the object is an instance of an S4 class.
}
\usage{
isS4(object)

asS4(object, value = TRUE)
}
\arguments{
  \item{object}{ Any R object. }

  \item{value}{A single logical value; not \code{NA}.}
}
\value{
  \code{isS4} always returns \code{TRUE} or \code{FALSE} according to
  whether the internal flag marking an S4 object has been turned on for
  this object.

  \code{asS4} will turn this flag on or off. But see the details.
}
\details{
  Note that \code{isS4} does not rely on the \pkg{methods}
  package, so in particular it can be used to detect the need to
  \code{\link{require}} that package.  (But \code{asS4} does depend on
  \pkg{methods}.)

  You should not set the flag directly unless you really know why.  As
  from \R 2.6.0, S4 methods are restricted to S4 objects for primitive
  functions; \code{asS4} allows method dispatch of S4 methods on
  primitives for S3 classes. For all other purposes, an object will
  satisfy \code{isS4(x)} if and only if it should.

  Note that S4 methods can only be set on those primitives which are
  \sQuote{\link{internal generic}} and \code{\%*\%}.
}
\seealso{\code{\link{is.object}}}
\examples{
\dontshow{require(methods)}
isS4(pi) # FALSE
isS4(getClass("MethodDefinition")) # TRUE
\dontshow{
stopifnot(isS4(asS4(Sys.time())))
stopifnot(!isS4(asS4(getClass("MethodDefinition"), FALSE)))
}
}
\keyword{programming}
