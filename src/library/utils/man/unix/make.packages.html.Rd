% File src/library/utils/man/unix/make.packages.html.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{make.packages.html}
\alias{make.packages.html}
\title{Update HTML Package List}
\usage{
make.packages.html(lib.loc = .libPaths(), temp = TRUE, verbose = TRUE)
}
\description{
  Re-create the HTML documentation files to reflect all available packages.
}
\arguments{
  \item{lib.loc}{character vector. List of libraries to be included.}
  \item{temp}{logical: should the package indices be created in a
    temporary location for use by the HTTP server?}
  \item{verbose}{logical: should messages and a heartbeat be shown?}
}
\details{
  This creates the \file{packages.html} file, either a temporary copy
  for use by \code{\link{help.start}}, or the copy in
  \file{R.home("doc")/html} (for which you will need write permission).

  It can be very slow, as all the package \file{DESCRIPTION} files in
  all the library trees are read.

  For \code{temp = TRUE} there is some caching of information, so the
  file will only be re-created if \code{lib.loc} or any of the
  directories it lists have been changed.
}
\value{
  Invisible logical, with \code{FALSE} indicating a failure to create
  the file, probably due to lack of suitable permissions.
}
\seealso{
  \code{\link{help.start}}
}
\examples{\dontrun{
# to prefer HTML help, put in your .Rprofile
options(help_type = "html")
make.packages.html(temp = FALSE)
# this can be slow for large numbers of installed packages.
}}
\keyword{utilities}
