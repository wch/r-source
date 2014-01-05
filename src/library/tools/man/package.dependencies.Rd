% File src/library/tools/man/package.dependencies.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{package.dependencies}
\alias{package.dependencies}
\title{Check Package Dependencies}
\description{
  Parses and checks the dependencies of a package against the currently
  installed version of R (and other packages).
}
\usage{
package.dependencies(x, check = FALSE,
                     depLevel =
                     c("Depends", "Imports", "Suggests"))
}
\arguments{
  \item{x}{A matrix of package descriptions as returned by
    \code{\link{available.packages}}.}
  \item{check}{If \code{TRUE}, return logical vector of check results.
    If \code{FALSE}, return parsed list of dependencies.}
  \item{depLevel}{Whether to look for \code{Depends} or \code{Suggests}
    level dependencies.}
}
\details{
  Currently we only check if the package conforms with the currently
  running version of R.  In the future we might add checks for
  inter-package dependencies.
}
\seealso{
  \code{\link{update.packages}}
}
\keyword{utilities}
