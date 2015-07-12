% File src/library/utils/man/chooseCRANmirror.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{chooseCRANmirror}
\alias{chooseCRANmirror}
\alias{getCRANmirrors}
\title{Select a CRAN Mirror}
\description{
  Interact with the user to choose a CRAN mirror.
}
\usage{
chooseCRANmirror(graphics = getOption("menu.graphics"), ind = NULL,
                 useHTTPS = getOption("useHTTPS", TRUE))

getCRANmirrors(all = FALSE, local.only = FALSE)
}
\arguments{
  \item{graphics}{Logical. If true, use a graphical list: on Windows or
    the OS X GUI use a list box, and on a Unix-alike if package
    \pkg{tcltk} and an X server are available, use a Tk
    widget. Otherwise use a text \code{\link{menu}}.}

  \item{ind}{Optional numeric value giving which entry to select.}
  \item{useHTTPS}{Whether to offer HTTPS mirrors.}
  \item{all}{Logical, get all known mirrors or only the ones flagged as OK.}
  \item{local.only}{Logical, try to get most recent list from CRAN or
    use file on local disk only.}
}
\details{
  A list of mirrors is stored in file
  \file{\var{\link{R_HOME}}/doc/CRAN_mirrors.csv}, but first an on-line
  list of current mirrors is consulted, and the file copy used only if
  the on-line list is inaccessible.

  This function was originally written to support a Windows GUI menu
  item, but is also called by \code{\link{contrib.url}} if it finds the
  initial dummy value of \code{\link{options}("repos")}.
  
  The \code{useHTTPS} argument defaults to \code{TRUE}.  With \code{useHTTPS=TRUE},
  HTTPS mirrors will be offered in preference to HTTP mirrors.  If it
  is set to \code{FALSE}, no HTTPS mirrors will be offered.  We recommend
  that it be left at the default value, as this will help to ensure that
  packages have not been tampered with in transit.
}
\value{
  None for \code{chooseCRANmirror()}, this function is invoked for its
  side effect of updating \code{options("repos")}.

  \code{getCRANmirrors()} returns a data frame with mirror information.
}
\seealso{
  \code{\link{setRepositories}}, \code{\link{chooseBioCmirror}},
  \code{\link{contrib.url}}.
}
\keyword{ utilities }
