% File src/library/base/man/lazyload.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{lazyLoad}
\alias{lazyLoad}
\alias{lazyLoadDBexec}
\title{Lazy Load a Database of R Objects}
\description{
  Internal functions to lazy load a database of \R objects.
}
\usage{
lazyLoad(filebase, envir = parent.frame(), filter)
lazyLoadDBexec(filebase, fun, filter)
}
\arguments{
  \item{filebase}{The file path to the database, with no extension.}
  \item{envir}{The environment into which objects are loaded.}
  \item{filter}{An optional function which when called on a a character
    vector of object names returns a logical vector: only objects for
    which this is true will be loaded.}
  \item{fun}{Function of one argument, an environment.}
}
\details{
  These are internal functions for use only by \R itself.

  The function \code{lazyLoad} is the workhorse function called by the
  package loader to load the code for a package from a database.  The
  database consists of two binary files, \file{\var{filebase}.rdb} (the
  objects) and \file{\var{filebase}.rdx} (an index).

  The objects are not themselves loaded into \code{envir}: rather
  promises are created that will load the object from the database on
  first access.  (See \code{\link{delayedAssign}}.)

  The function \code{lazyLoadDBexec} contains the core implementation
  and is also used by the mechanism for loading processed help file
  data.
}
\author{Luke Tierney}
\keyword{internal}
