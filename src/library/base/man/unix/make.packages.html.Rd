\name{make.packages.html}
\alias{make.packages.html}
\title{Update HTML documentation files}
\usage{
make.packages.html(lib.loc = .libPaths())
}
\description{
  Functions to re-create the HTML documentation files to reflect all
  installed packages.
}
\arguments{
  \item{lib.loc}{character vector. List of libraries to be included.}
}
\details{
  This sets up the links from packages in libraries to the \file{.R}
  subdirectory of the per-session directory (see \code{\link{tempdir}})
  and then creates the \file{packages.html} and \file{index.txt} files
  to point to those links.

  If a package is available in more than one library tree, all the
  copies are linked, after the first with suffix \code{.1} etc.
}
\value{
  Logical, whether the function succeeded in recreating the files.
}
\seealso{
  \code{\link{help.start}}
}
\keyword{utilities}
