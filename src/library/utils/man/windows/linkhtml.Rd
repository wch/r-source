% File src/library/utils/man/windows/linkhtml.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Distributed under GPL 2 or later

\name{make.packages.html}
\alias{make.packages.html}
\title{Update HTML documentation files}
\usage{
make.packages.html(lib.loc = .libPaths(), temp = FALSE, verbose = TRUE)
}
\description{
  Re-create the HTML list of packages.
}
\arguments{
  \item{lib.loc}{character vector.  List of libraries to be included.}
  \item{temp}{logical: should the package indices be created in a
    temporary location for use by the HTTP server?}
  \item{verbose}{logical.  If true, print out a message and use a
    progress bar.}
}
\details{
  This creates the \file{packages.html} file, either a temporary copy
  for use by \code{\link{help.start}}, or the copy in
  \file{\var{\link{R_HOME}}\\doc\\html} (for which you will
  need write permission).
  It can be very slow, as all the package \file{DESCRIPTION} files in
  all the library trees are read.

  For \code{temp = TRUE} there is some caching of information, so the
  file will only be re-created if \code{lib.loc} or any of the
  directories it lists have been changed.
}

\value{
  Logical value showing if it succeeded.
}

\keyword{utilities}
