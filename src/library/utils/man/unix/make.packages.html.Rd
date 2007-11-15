% File src/library/utils/man/unix/make.packages.html.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{make.packages.html}
\alias{make.packages.html}
\title{Update HTML Documentation Files}
\usage{
make.packages.html(lib.loc = .libPaths(), packages = TRUE)
}
\description{
  Functions to link and (optionally) re-create the HTML documentation
  files to reflect all installed packages.
}
\arguments{
  \item{lib.loc}{character vector. List of libraries to be included.}
  \item{packages}{logical: should be package indices be create.}
}
\details{
  This sets up the links from packages in libraries to the \file{.R}
  subdirectory of the per-session directory (see \code{\link{tempdir}})
  and optionally creates the \file{packages.html} and \file{index.txt} files
  to point to those links.  The slow part is creating those files (as
  all the package \file{DESCRIPTION} files are read).

  If a package is available in more than one library tree, all the
  copies are linked, after the first with suffix \code{.1} etc.
}
\value{
  Invisible logical, indicating if the files were created.
}
\seealso{
  \code{\link{help.start}}
}
\examples{\dontrun{
# to prefer HTML help, put in your .Rprofile
options(htmlhelp=TRUE)
make.packages.html(packages=FALSE)
# this can be slow for large numbers of installed packages.
}}
\keyword{utilities}
