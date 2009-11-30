% File src/library/base/man/isS4.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{isS4}
\alias{isS4}
\alias{asS4}
\alias{S4}

\title{ Test for an S4 object }
\description{
  Tests whether the object is an instance of an S4 class.
}
\usage{
isS4(object)

asS4(object, flag = TRUE, complete = TRUE)
}
\arguments{
  \item{object}{ Any R object. }

  \item{flag}{A single logical value; not \code{NA}, whether to turn
      the S4 object flag on or off.}
  \item{complete}{How to complete the transformation to/from S4.  Only
    currently used when \code{flag} is \code{FALSE}.  If \code{1}, the object
    must convert to a valid S3 object, or an error results.  If \code{2}, a
    non-valid S3 object is left alone, silently.  If \code{0}, no conversion
    is made other than setting the internal bit (used internally but not recommended, since it can result
    in an invalid object).
  }
}
\value{
  \code{isS4} always returns \code{TRUE} or \code{FALSE} according to
  whether the internal flag marking an S4 object has been turned on for
  this object.

  \code{asS4} will turn this flag on or off, according to argument
  \code{flag}, and in the latter case complete the conversion as
  described under argument \code{complete}. Note that with \code{flag
    FALSE}, an S4 object will \emph{not} but turned into an S3 object
  unless there is a valid conversion; that is, an object of type other
  than \code{"S4"} for which the S4 object is an extension, unless
  argument \code{complete} is \code{0}.
}
\details{
  Note that \code{isS4} does not rely on the \pkg{methods}
  package, so in particular it can be used to detect the need to
  \code{\link{require}} that package.  (But \code{asS4} does depend on
  \pkg{methods}.)
  
  When \code{asS4} is called with \code{flag == FALSE}, the value of
  \code{complete} controls whether an attempt is made to transform
  \code{object} into a valid object of the implied S3 class.  If so,
  then an object from an S4 class extending an S3 class will be
  transformed into an S3 object with the corresponding S3 class (see
  \code{\link{S3Part}}).  This includes classes extending the
  pseudo-classes \code{array} and \code{matrix}: such objects will have
  their class attribute set to \code{NULL}.
}
\seealso{\code{\link{is.object}} for a more general test; \link{Methods}
for general information on S4.}
\examples{
\dontshow{require(methods)}
isS4(pi) # FALSE
isS4(getClass("MethodDefinition")) # TRUE
\dontshow{
stopifnot(isS4(asS4(Sys.time())))
## Following is a correction of previous behavior. See note in the
## value section above 
stopifnot(isS4(asS4(getClass("MethodDefinition"), FALSE, 2)))
stopifnot(!isS4(asS4(getClass("MethodDefinition"), FALSE, 0)))
}
}
\keyword{programming}
