\name{LINK}
\alias{LINK}
\title{Create Executable Programs}
\description{
  Front-end for creating executable programs.
}
\usage{
\special{R CMD LINK [options] linkcmd}
}
\arguments{
  \item{linkcmd}{a list of commands to link together suitable object
    files (include library objects) to create the executable program.}
  \item{options}{further options to control the linking, or for
    obtaining information about usage and version.}
}
\details{
  The linker front-end is useful in particular when linking against the
  R shared library, in which case \code{linkcmd} must contain \code{-lR}
  but need not specify its library path.
  
  Currently only works if the C compiler is used for linking, and no C++
  code is used.

  Use \code{R CMD LINK --help} for more usage information.
}
\note{
  Some binary distributions of \R have \code{LINK} in a separate
  bundle, e.g. an \code{R-devel} RPM.
}
\keyword{utilities}
