\name{undoc}
\alias{undoc}
\title{Find Undocumented Objects}
\usage{
undoc(pkg, dir)
}
\description{
  Finds the objects in a package which are undocumented, in the sense
  that they are visible to the user (or data objects provided by the
  package), but no documentation entry exists.
}
\arguments{
  \item{pkg}{a character string naming an installed package}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This must contain the subdirectory \file{man}
    with \R documentation sources (in Rd format), and at least one of
    the \file{R} or \file{data} subdirectories with \R code or data
    objects, respectively.}
}
\details{
  This function is useful for package maintainers mostly.  In principle,
  \emph{all} user level R objects should be documented; note however
  that the precise rules for documenting methods of generic functions
  are still under discussion.
}
\value{
  A character vector containing the names of the undocumented objects.
}
\examples{
undoc("eda")                   # Undocumented objects in `eda'
}
\keyword{documentation}
