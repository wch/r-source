% File src/library/utils/man/download.packages.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{download.packages}
\alias{download.packages}
\title{Download Packages from CRAN-like Repositories}
\description{
  These functions can be used to automatically compare the version
  numbers of installed packages with the newest available version on
  the repositories and update outdated packages on the fly.
}
\usage{
download.packages(pkgs, destdir, available = NULL,
                  repos = getOption("repos"),
                  contriburl = contrib.url(repos, type),
                  method, type = getOption("pkgType"), \dots)
}
\arguments{
  \item{pkgs}{
    character vector of the names of packages whose latest available
    versions should be downloaded from the repositories.
  }
  \item{destdir}{
    directory where downloaded packages are to be stored.
  }
  \item{available}{
    an object as returned by \code{\link{available.packages}}
    listing packages available at the repositories, or \code{NULL} which
    makes an internal call to \code{available.packages}.
  }
  \item{repos}{
    character vector, the base URL(s) of the repositories
    to use, i.e., the URL of the CRAN master such as
    \code{"https://cran.r-project.org"} or its Statlib mirror,
    \code{"http://lib.stat.cmu.edu/R/CRAN"}.
  }
  \item{contriburl}{
    URL(s) of the contrib sections of the
    repositories.  Use this argument only if your repository mirror is
    incomplete, e.g., because you burned only the \file{contrib} section on a
    CD.  Overrides argument \code{repos}.
  }
  \item{method}{
    Download method, see \code{\link{download.file}}.
  }
  \item{type}{
    character string, indicate which type of packages: see
    \code{\link{install.packages}} and \sQuote{Details}.
  }
  \item{\dots}{
    additional arguments to be passed to \code{\link{download.file}}
    and \code{\link{available.packages}}.
  }
}
\details{
  \code{download.packages} takes a list of package names and a
  destination directory, downloads the newest versions and saves them in
  \code{destdir}.  If the list of available packages is not given as
  argument, it is obtained from repositories.  If a repository is local,
  i.e.\sspace{}the URL starts with \code{"file:"}, then the packages are not
  downloaded but used directly.  Both \code{"file:"} and
  \code{"file:///"} are allowed as prefixes to a file path.  Use the
  latter only for URLs: see \code{\link{url}} for their interpretation.
  (Other forms of \samp{file://} URLs are not supported.)

  For \code{download.packages}, \code{type = "both"} looks at source
  packages only.
}
\value{
  A two-column matrix of names and destination file names of those
  packages successfully downloaded.  If packages are not available or
  there is a problem with the download, suitable warnings are given.
}
\seealso{
  \code{\link{available.packages}}, \code{\link{contrib.url}}.

  The main use is by \code{\link{install.packages}}.

  See \code{\link{download.file}} for how to handle proxies and
  other options to monitor file transfers.

  The \sQuote{R Installation and Administration} manual for how to
  set up a repository.
}

\keyword{utilities}
