% File src/library/utils/man/update.packages.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{contrib.url}
\alias{contrib.url}
\title{Find Appropriate Paths in CRAN-like Repositories}
\description{
  \code{contrib.url} adds the appropriate type-specific path within a
  repository to each URL in \code{repos}.
}
\usage{
contrib.url(repos, type = getOption("pkgType"))
}
\arguments{
  \item{repos}{character vector, the base URL(s) of the repositories
    to use.
  }
  \item{type}{character string, indicating which type of packages: see
    \code{\link{install.packages}}.
  }
}
\value{
  A character vector of the same length as \code{repos}.
}
\seealso{
  \code{\link{available.packages}}, \code{\link{download.packages}},
  \code{\link{install.packages}}.

  The \sQuote{R Installation and Administration} manual for how to
  set up a repository.
}
\keyword{utilities}
