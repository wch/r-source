% File src/library/base/man/as.environment.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
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
as.environment(x)
}
\arguments{
  \item{x}{an \R object to convert.  If it is already an
    environment, just return it.  If it is a positive number, return the
    environment corresponding to that position on the search list. If it
    is \code{-1}, the environment it is called from.  If
    it is a character string, match the string to the names on the
    search list.

    If it is a list, the equivalent of \code{\link{list2env}(x,
      parent = emptyenv())} is returned.

    If \code{\link{is.object}(x)} is true and it has a \code{\link{class}}
    for which an \code{as.environment} method is found, that is used.
  }
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
try( ## <<- stats need not be attached
    as.environment("package:stats"))
ee <- as.environment(list(a = "A", b = pi, ch = letters[1:8]))
ls(ee) # names of objects in ee
utils::ls.str(ee)
}
\keyword{data}
\keyword{environment}
