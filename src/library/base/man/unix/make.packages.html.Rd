\name{make.packages.html}
\alias{make.packages.html}
\title{Update HTML documentation files}
\usage{
make.packages.html(lib.loc = .libPaths())
}
\description{
  Functions to re-create the HTML documentation files to reflect all
  installed packages. \code{make.packages.html} creates
  the list of packages.
}
\arguments{
  \item{lib.loc}{character vector. List of libraries to be included.}
}
\value{
  Logical, whether the function succeeded in recreating the file.
}
\author{Brian Ripley}
\keyword{utilities}
