% File src/library/base/man/as.environment.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Development Team
% Distributed under GPL 2 or later

\name{as.environment}
\alias{as.environment}
\title{ Coerce to an Environment Object }
\description{
  A generic function coercing an \R object to an
  \code{\link{environment}}.  A number or a character string is
  converted to the corresponding environment on the search path.
}
\usage{
as.environment(object)
}
\arguments{
  \item{object}{an \R object to convert.  If it is already an
    environment, just return it.  If it is a number, return the
    environment corresponding to that position on the search list.  If
    it is a character string, match the string to the names on the
    search list.

    If it is a list, the equivalent of \code{\link{list2env}(object,
      parent=emptyenv())} is returned.

    If \code{\link{is.object}(object)} is true and it has a
    \code{\link{class}} for which an \code{as.environment} method is
    found, that is used.}
}
\value{
  The corresponding environment object.
}
\note{
  This is a \link{primitive} function.
}
\author{ John Chambers }
\seealso{\code{\link{environment}} for creation and manipulation,
  \code{\link{search}}; \code{\link{list2env}}.
}
\examples{
as.environment(1) ## the global environment
identical(globalenv(), as.environment(1)) ## is TRUE
try( ## <<- stats need not be loaded
    as.environment("package:stats"))
}
\keyword{data}
\keyword{environment}
