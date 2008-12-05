% File src/library/utils/man/aqua/browse.pkgs.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{browse.pkgs}
\alias{browse.pkgs}
\title{Download R packages}
\description{
  Visual browser for downloading \R binary or source packages from a
  repository, usually invoked from the \sQuote{Packages} menu of
  \samp{R.app}.
}
\usage{
browse.pkgs(repos = getOption("repos"),
            contriburl = contrib.url(repos, type),
            type = getOption("pkgType"))
}
\arguments{
  \item{repos}{repository: see \code{\link{install.packages}}.}
  \item{contriburl}{alternative to \code{repos} and \code{type}: see
    \code{\link{install.packages}}.}
  \item{type}{type of package: see \code{\link{install.packages}}.}
}
\seealso{
  \code{install.packages}
}
\keyword{connection}
\keyword{utilities}
