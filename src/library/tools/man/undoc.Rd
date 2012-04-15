% File src/library/tools/man/undoc.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{undoc}
\alias{undoc}
\alias{print.undoc}
\title{Find Undocumented Objects}
\usage{
undoc(package, dir, lib.loc = NULL)
}
\description{
  Finds the objects in a package which are undocumented, in the sense
  that they are visible to the user (or data objects or S4 classes
  provided by the package), but no documentation entry exists.
}
\arguments{
  \item{package}{a character string naming an installed package.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This must contain the subdirectory \file{man}
    with \R documentation sources (in Rd format), and at least one of
    the \file{R} or \file{data} subdirectories with \R code or data
    objects, respectively.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    search for \code{package}.}
}
\details{
  This function is useful for package maintainers mostly.  In principle,
  \emph{all} user-level \R objects should be documented.

  The \pkg{base} package is special as it contains the primitives and
  these do not have definitions available at code level.  We provide
  equivalent closures in environments \code{.ArgsEnv} and
  \code{.GenericArgsEnv} in the \pkg{base} package that are used
  for various purposes: \code{undoc("base")} checks that all the
  primitives that are not language constructs are prototyped in those
  environments and no others are.
}
\value{
  An object of class \code{"undoc"} which is a list of character vectors
  containing the names of the undocumented objects split according to
  documentation type.

  There is a \code{print} method for nicely displaying the information
  contained in such objects.
}
\seealso{
  \code{\link{codoc}}, \code{\link{QC}}
}
\examples{
undoc("tools")                  # Undocumented objects in 'tools'
}
\keyword{documentation}
