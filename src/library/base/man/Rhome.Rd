\name{R.home}
\alias{R.home}
\alias{R_HOME}
\title{Return the R Home Directory}
\description{
  Return the \R home directory.
}
\usage{
R.home(component="home")
}
\arguments{
  \item{component}{As well as \code{"home"} which gives the \R home
    directory, other known values are \code{"bin"}, \code{"doc"},
    \code{"etc"} and \code{"share"} giving the paths to the
    corresponding parts of an \R installation.}
}
\details{
  The \R home directory is the top-level directory of the \R
  installation being run.

  The \R home directory is often referred to as \code{R_HOME},
  and is the value of an environment variable of that name in an \R
  session.
#ifdef unix
  It can be found outside an \R session by \code{R \link{RHOME}}.
#endif  
}
\value{
  A character string giving the \R home directory or path to a
  particular component.  Normally the components are all subdirectories
  of the \R home directory, but this may not be the case in a Unix-like
  installation.
}
\keyword{utilities}
