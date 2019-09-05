% File src/library/tools/man/add_datalist.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011-2019 R Core Team
% Distributed under GPL 2 or later

\name{add_datalist}
\alias{add_datalist}
\title{
  Add a \file{datalist} File to a Source Package
}
\description{
  The \code{\link{data}()} command with no arguments lists all the
  datasets available via \code{data} in attached packages, and to do so a
  per-package list is installed.  Creating that list at install time can
  be slow for packages with huge datasets, and can be expedited by a
  supplying \file{data/datalist} file.
}
\usage{
add_datalist(pkgpath, force = FALSE, small.size = 1024^2)
}
\arguments{
  \item{pkgpath}{The path to a (source) package.}

  \item{force}{logical: can an existing \file{data/datalist} file be
    over-written?}

  \item{small.size}{number: a \file{data/datalist} file is created only if
    the total size of the data files is larger than \code{small.size} bytes.}
}
\details{
  \command{R CMD build} will call this function to add a data list to
  packages with 1MB or more of file in the \file{data} directory.

  It can also be also helpful to give a \file{data/datalist} file in
  packages whose datasets have many dependencies, including loading the
  packages itself (and maybe others).
}
\seealso{
  \code{\link{data}}.

  The \sQuote{Writing R Extensions} manual.
}
\keyword{utilities}
