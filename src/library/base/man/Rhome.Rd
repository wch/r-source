\name{R.home}
\alias{R.home}
\title{Return the R Home Directory}
\description{
 Return the \R home directory.
}
\usage{
R.home(component="home")
}
\arguments{
  \item{component}{As well as \code{"home"} which gives \the \R home
    directory, other known values are \code{"bin"}, \code{"doc"},
    \code{"etc"} and \code{"share"} giving the paths to the
    corresponding parts of an \R installation.}
}
\value{
  A character string giving the current \R home directory or path to a
  particular component.  Currently the components are all subdirectories
  of the \R home directory, but this may change.
}
\keyword{utilities}
