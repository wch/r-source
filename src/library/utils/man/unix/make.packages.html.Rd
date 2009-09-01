% File src/library/utils/man/unix/make.packages.html.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{make.packages.html}
\alias{make.packages.html}
\title{Update HTML Package List}
\usage{
make.packages.html(lib.loc = .libPaths(), temp = TRUE)
}
\description{
  Re-create the HTML documentation files to reflect all available packages.
}
\arguments{
  \item{lib.loc}{character vector. List of libraries to be included.}
  \item{temp}{logical: should the package indices be created in a
    temporary location?}
}
\details{
  This creates the \file{packages.html} file, either a temprorary copy
  for use by \code{\link{help.start}}, or the copy in
  \file{R.home("doc")/html}.

  It can be slow, as all the package \file{DESCRIPTION} files are read.
}
\value{
  Invisible logical, indicating if the file was created.
}
\seealso{
  \code{\link{help.start}}
}
\examples{\dontrun{
# to prefer HTML help, put in your .Rprofile
options(htmlhelp=TRUE)
make.packages.html(temp = FALSE)
# this can be slow for large numbers of installed packages.
}}
\keyword{utilities}
