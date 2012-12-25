% File src/library/base/man/isS4.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{isS4}
\alias{isS4}
\alias{asS4}
\alias{S4}
\alias{asS3}

\title{ Test for an S4 object }
\description{
  Tests whether the object is an instance of an S4 class.
}
\usage{
isS4(object)

asS4(object, flag = TRUE, complete = TRUE)
asS3(object, flag = TRUE, complete = TRUE)
}
\arguments{
  \item{object}{Any R object.}

  \item{flag}{Optional, logical: indicate direction of conversion.}

  \item{complete}{Optional, logical: whether conversion to S3 is
    completed.  Not usually needed, but see the details section.}
}

\details{
  Note that \code{isS4} does not rely on the \pkg{methods}
  package, so in particular it can be used to detect the need to
  \code{\link{require}} that package.

  \code{asS3} uses the value of
  \code{complete} to control whether an attempt is made to transform
  \code{object} into a valid object of the implied S3 class.  If
  \code{complete} is \code{TRUE},
  then an object from an S4 class extending an S3 class will be
  transformed into an S3 object with the corresponding S3 class (see
  \code{\link{S3Part}}).  This includes classes extending the
  pseudo-classes \code{array} and \code{matrix}: such objects will have
  their class attribute set to \code{NULL}.

  \code{isS4} is \link{primitive}.
}

\value{
  \code{isS4} always returns \code{TRUE} or \code{FALSE} according to
  whether the internal flag marking an S4 object has been turned on for
  this object.

  \code{asS4} and \code{asS3} will turn this flag on or off,
  and \code{asS3} will set the class from the objects \code{.S3Class}
  slot if one exists.  Note that \code{asS3} will \emph{not}  turn
  the object into an S3 object
  unless there is a valid conversion; that is, an object of type other
  than \code{"S4"} for which the S4 object is an extension, unless
  argument \code{complete} is \code{FALSE}.
}

\seealso{
  \code{\link{is.object}} for a more general test; \link{Methods}
  for general information on S4.
}

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
