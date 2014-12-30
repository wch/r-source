% File src/library/utils/man/setRepositories.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{setRepositories}
\alias{setRepositories}
\title{Select Package Repositories}
\description{
  Interact with the user to choose the package repositories to be used.
}
\usage{
setRepositories(graphics = getOption("menu.graphics"),
                ind = NULL, addURLs = character())
}
\arguments{
  \item{graphics}{Logical. If true, use a graphical list: on Windows or
    OS X GUI use a list box, and on a Unix-alike if \pkg{tcltk} and an X
    server are available, use Tk widget. Otherwise use a text
    \code{\link{menu}}.}
  \item{ind}{\code{NULL} or a vector of integer indices, which have the
    same effect as if they were entered at the prompt for
    \code{graphics = FALSE}.}
  \item{addURLs}{A character vector of additional URLs: it is often
    helpful to use a named vector.}
}
\details{
  The default list of known repositories is stored in the file
  \file{\var{\link{R_HOME}}/etc/repositories}.
  That file can be edited for a site, or a user can have a personal copy
  in \file{\var{HOME}/.R/repositories} which will take precedence.

  A Bioconductor mirror can be selected by setting
  \code{\link{options}("BioC_mirror")}, e.g.\sspace{}via
  \code{\link{chooseBioCmirror}} the default value is
  \samp{"http://www.bioconductor.org"}.

  The items that are preselected are those that are currently in
  \code{options("repos")} plus those marked as default in the
  list of known repositories.

  The list of repositories offered depends on the setting of option
  \code{"pkgType"} as some repositories only offer a subset of types
  (e.g., only source packages or not OS X binary packages).
  Further, for binary packages some repositories (notably R-Forge) only
  offer packages for the current or recent versions of \R.
  (Type \code{"both"} is equivalent to \code{"source"}.)

  Repository \samp{CRAN} is treated specially: the value is taken from
  the current setting of \code{getOption("repos")} if this has an
  element \code{"CRAN"}: this ensures mirror selection is sticky.

  This function requires the \R session to be interactive unless
  \code{ind} is supplied.
}
\note{
  This does \strong{not} set the list of repositories at startup: to do
  so set \code{\link{options}(repos =)} in a start up file (see help topic
  \link{Startup}).
}
\value{
  This function is invoked mainly for its side effect of updating
  \code{options("repos")}.  It returns (invisibly) the previous
  \code{repos} options setting (as a \code{\link{list}} with component
  \code{repos}) or \code{\link{NULL}} if no changes were applied.
}
\seealso{
  \code{\link{chooseCRANmirror}}, \code{\link{chooseBioCmirror}},
  \code{\link{install.packages}}.
}
\examples{\dontrun{
setRepositories(addURLs =
                c(CRANxtras = "http://www.stats.ox.ac.uk/pub/RWin"))
}}
\keyword{ utilities }
