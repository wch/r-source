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
    to search for \code{package}.}
}
\details{
  This function is useful for package maintainers mostly.  In principle,
  \emph{all} user level \R objects should be documented; note however
  that the precise rules for documenting methods of generic functions
  are still under discussion.
}
\value{
  An object of class \code{"undoc"} which is a list of character vectors
  containing the names of the undocumented objects split according to
  documentation type.  This representation is still experimental, and
  might change in future versions.

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
