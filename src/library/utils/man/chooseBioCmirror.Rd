% File src/library/utils/man/chooseBioCmirror.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{chooseBioCmirror}
\alias{chooseBioCmirror}
\alias{getBioCmirrors}
\title{Select a Bioconductor Mirror}
\description{
  Interact with the user to choose a Bioconductor mirror.
}
\usage{
chooseBioCmirror(graphics = getOption("menu.graphics"))
}
\arguments{
  \item{graphics}{Logical. If true, use a graphical list: on Windows or
    Mac OS X GUI use a list box, and on a Unix-alike if package
    \pkg{tcltk} and an X server are available, use a Tk
    widget. Otherwise use a text \code{\link{menu}}.}
}
\details{
  This sets the option \code{"BioC_mirror"}: it needs to be used before
  a call to \code{\link{setRepositories}}.

  Currently the Bioconductor master site (in Seattle, USA), NIH
  (Bethesda, USA) and the European mirror in Dortmund, Germany are
  available to select from.
}
\value{
  None: this function is invoked for its
  side effect of updating \code{options("BioC_mirror")}.
}
\seealso{
  \code{\link{setRepositories}}, \code{\link{chooseCRANmirror}}.
}
\keyword{ utilities }
