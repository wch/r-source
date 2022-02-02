% File src/library/utils/man/chooseBioCmirror.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2017 R Core Team
% Distributed under GPL 2 or later

\name{chooseBioCmirror}
\alias{chooseBioCmirror}
\alias{getBioCmirrors}
\title{Select a Bioconductor Mirror}
\description{
  Interact with the user to choose a Bioconductor mirror.
}
\usage{
chooseBioCmirror(graphics = getOption("menu.graphics"), ind = NULL,
                 local.only = FALSE)
}
\arguments{
  \item{graphics}{Logical.  If true, use a graphical list: on Windows or
    the macOS GUI use a list box, and on a Unix-alike use a Tk widget if
    package \pkg{tcltk} and an X server are available.  Otherwise use a
    text \code{\link{menu}}.}
  
  \item{ind}{Optional numeric value giving which entry to select.}
  
  \item{local.only}{Logical, try to get most recent list from the
    Bioconductor master or use file on local disk only.}
}
\details{
  This sets the \link{option} \code{"BioC_mirror"}: it is used
  before a call to \code{\link{setRepositories}}.  The out-of-the-box
  default for that option is \code{NULL}, which currently corresponds to
  the mirror \url{https://bioconductor.org}.

  The \sQuote{Bioconductor (World-wide)} \sQuote{mirror} is a network of
  mirrors providing reliable world-wide access; other mirrors may
  provide faster access on a geographically local scale.

  \code{ind} chooses a row in
  \file{\var{\link{R_HOME}}/doc/BioC_mirrors.csv},
  by number.
}
\value{
  None: this function is invoked for its
  side effect of updating \code{options("BioC_mirror")}.
}
\seealso{
  \code{\link{setRepositories}}, \code{\link{chooseCRANmirror}}.
}
\keyword{ utilities }
