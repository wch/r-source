% File src/library/utils/man/unix/make.packages.html.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{make.packages.html}
\alias{make.packages.html}
\title{Update HTML Package List}
\usage{
make.packages.html(lib.loc = .libPaths(), temp = FALSE,
                   verbose = TRUE, docdir = R.home("doc"))
}
\description{
  Re-create the HTML list of packages.
}
\arguments{
  \item{lib.loc}{character vector.  List of libraries to be included.}
  \item{temp}{logical: should the package indices be created in a
    temporary location for use by the HTTP server?}
  \item{verbose}{logical.  If true, print out a message.
#ifdef windows
    On Windows, show a progress bar.
#endif
  }
  \item{docdir}{If \code{temp} is false, directory in whose \file{html}
    directory the \file{packages.html} file is to be created/updated.}
}
\details{
  This creates the \file{packages.html} file, either a temporary copy
  for use by \code{\link{help.start}}, or the copy in
#ifdef unix
  \file{R.home("doc")/html}
#endif
#ifdef windows
  \file{\var{\link{R_HOME}}\\doc\\html}
#endif
  (for which you will need write permission).

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
make.packages.html()
# this can be slow for large numbers of installed packages.
}}

\keyword{utilities}
