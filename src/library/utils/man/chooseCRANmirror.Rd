% File src/library/utils/man/chooseCRANmirror.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
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
                 local.only = FALSE)

getCRANmirrors(all = FALSE, local.only = FALSE)
}
\arguments{
  \item{graphics}{Logical.  If true, use a graphical list: on Windows or
    the macOS GUI use a list box, and on a Unix-alike  use a Tk widget if
    package \pkg{tcltk} and an X server are available.  Otherwise use a
    text \code{\link{menu}}.}
  
  \item{ind}{Optional numeric value giving which entry to select.}
  \item{all}{Logical, get all known mirrors or only the ones flagged as OK.}
  \item{local.only}{Logical, try to get most recent list from the CRAN
    master or use file on local disk only.}
}
\details{
  A list of mirrors is stored in file
  \file{\var{\link{R_HOME}}/doc/CRAN_mirrors.csv}, but first an on-line
  list of current mirrors is consulted, and the file copy used only if
  the on-line list is inaccessible.

  This function is called by a Windows GUI menu item and by
  \code{\link{contrib.url}} if it finds the initial dummy value of
  \code{\link{options}("repos")}.
  
  HTTPS mirrors with mirroring over \command{ssh} will be offered in
  preference to other mirrors (which are listed in a sub-menu).

  \code{ind} chooses a row in the list of current mirrors, by number. It
  is best used with \code{local.only = TRUE} and row numbers in
   \file{\var{\link{R_HOME}}/doc/CRAN_mirrors.csv}.
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
