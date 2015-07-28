% File src/library/utils/man/getS3method.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{getS3method}
\alias{getS3method}
\title{Get an S3 Method}
\description{
  Get a method for an S3 generic, possibly from a namespace or the
  generic's registry.
}
\usage{
getS3method(f, class, optional = FALSE, envir = parent.frame())
}
\arguments{
  \item{f}{character: name of the generic.}
  \item{class}{character: name of the class.}
  \item{optional}{logical: should failure to find the generic or a
    method be allowed?}
  \item{envir}{the \code{\link{environment}} in which the method and its
    generic are searched first.}
}
\details{
  S3 methods may be hidden in namespaces, and will not
  then be found by \code{\link{get}}: this function can retrieve
  such functions, primarily for debugging purposes.

  Further, S3 methods can be registered on the generic when a namespace
  is loaded, and the registered method will be used if none is visible
  (using namespace scoping rules).

  It is possible that which S3 method will be used may depend on where
  the generic \code{f} is called from: \code{getS3method} returns the
  method found if \code{f} were called from the same environment.
}
\value{
  The function found, or \code{NULL} if no function is found and
  \code{optional = TRUE}.
}
\seealso{
  \code{\link{methods}}, \code{\link{get}}, \code{\link{getAnywhere}}
}
\examples{
require(stats)
exists("predict.ppr") # false
getS3method("predict", "ppr")
}
\keyword{data}
\keyword{methods}
