\name{lazyLoad}
\alias{lazyLoad}
\title{Lazy Load a Database of R Objects}
\description{
  Lazy load a database of \R objects.
}
\usage{
lazyLoad(filebase, envir = parent.frame(), filter)
}
\arguments{
  \item{filebase}{The file path to the database, with no extension.}
  \item{envir}{The environment into which objects are loaded.}
  \item{filter}{An optional function which when called on an object name
    returns \code{TRUE} or \code{FALSE}: only objects for which this is
    true will be loaded.}
}
\details{
  This is the workhorse function called by the package loader to load
  the code for a package from a database.  The database consists of two
  binary files, \code{filebase.rdb} (the objects) and \code{filebase.rdx}
  (an index).

  The objects are not themselves loaded into \code{envir}: rather
  promises are created that will load the object from the database on
  first access. (See \code{delay}.)
}
\author{Luke Tierney}
\seealso{
  \code{\link{makeLazyLoading}}
}
\keyword{internal}
