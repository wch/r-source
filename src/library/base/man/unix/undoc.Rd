\name{undoc}
\alias{undoc}
\title{Find Undocumented Objects}
\usage{
undoc(pkg, dir, code.dir, docs.dir)
}
\arguments{
  \item{pkg}{a character string naming an installed package}
  \item{dir}{a character string specifying the path to a package's root
    source directory (containing the subdirectories \file{R} with R code
    and \file{man} with Rd documentation sources, respectively.}
  \item{code.dir}{a character string specifying the path to a package's
    R code source (\code{R}) directory.}
  \item{docs.dir}{a character string specifying the path to a package's
    Rd documentation source (\code{man}) directory.}
}
\description{
  Finds the objects in a package which are undocumented, in the sense
  that they are visible to the user but no documentation entry exists.
}
\details{
  This function is useful for package maintainers mostly.  In principle,
  \emph{all} user level R objects should be documented; note however
  that the precise rules for documenting methods of generic functions
  are still under discussion.

  Currently, it is not checked whether documentation exists for all data
  objects the package provides.
}
\value{
  A character vector containing the names of the undocumented objects.
}
\examples{
undoc("eda")                   # Undocumented objects in `eda'
}
\keyword{documentation}
