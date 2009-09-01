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
make.packages.html(lib.loc = .libPaths(), temp = FALSE)
}
\description{
  Functions to re-create the HTML documentation files to reflect all
  installed packages. \code{make.packages.html} creates
  the list of packages, and \code{link.html.help} is a more verbose wrapper.
}
\arguments{
  \item{verbose}{logical.  If true, print out a message. For use to
    explain a delay when called from other functions.}
  \item{lib.loc}{character vector.  List of libraries to be included.}
    \item{temp}{logical: should the package indices be created in a
    temporary location?}
}
\value{
  \code{make.packages.html} returns a logical showing if it succeeded.
  
  The file
  \file{\var{\link[base:Rhome]{R_HOME}}\\doc\\html\\packages.html} is
  recreated by \code{make.packages.html(outfile = NULL)}.
}
\note{
  You will need write permission in the
  \file{\var{\link[base:Rhome]{R_HOME}}\\doc\\html}
  directory to use these functions (unless \code{temp = FALSE}).
}
\keyword{utilities}
