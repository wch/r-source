% File src/library/utils/man/chooseBioCmirror.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-2013 R Core Team
% Distributed under GPL 2 or later

\name{chooseBioCmirror}
\alias{chooseBioCmirror}
\alias{getBioCmirrors}
\title{Select a Bioconductor Mirror}
\description{
  Interact with the user to choose a Bioconductor mirror.
}
\usage{
chooseBioCmirror(graphics = getOption("menu.graphics"), ind = NULL)
}
\arguments{
  \item{graphics}{logical.  If true, use a graphical list: on Windows or
    the OS X GUI use a list box, and on a Unix-alike if package
    \pkg{tcltk} and an X server are available, use a Tk
    widget.  Otherwise use a text \code{\link{menu}}.}
  
  \item{ind}{Optional numeric value giving which entry to select.}
}
\details{
  This sets the option \code{"BioC_mirror"}: it needs to be used before
  a call to \code{\link{setRepositories}}.

  In addition to the Bioconductor master site (in Seattle, USA), there
  currently are mirrors in Bethesda (USA), Dortmund (Germany), Bergen
  (Norway) and Cambridge (UK).
}
\value{
  None: this function is invoked for its
  side effect of updating \code{options("BioC_mirror")}.
}
\seealso{
  \code{\link{setRepositories}}, \code{\link{chooseCRANmirror}}.
}
\keyword{ utilities }
