% File src/library/utils/man/windows/linkhtml.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{link.html.help}
\alias{link.html.help}
\alias{make.packages.html}
\title{Update HTML documentation files}
\usage{
link.html.help(verbose = FALSE, lib.loc = .libPaths())
make.packages.html(lib.loc = .libPaths(), temp = FALSE, verbose = TRUE)
}
\description{
  Functions to re-create the HTML documentation files to reflect all
  installed packages. \code{make.packages.html} creates
  the list of packages, and \code{link.html.help} is a more verbose wrapper.
}
\arguments{
  \item{verbose}{logical.  If true, print out a message and use a
    progress bar.}
  \item{lib.loc}{character vector.  List of libraries to be included.}
  \item{temp}{logical: should the package indices be created in a
    temporary location for use by the HTTP server?}
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
  \code{make.packages.html} returns a logical showing if it succeeded.
}

\keyword{utilities}
