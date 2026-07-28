% File src/library/base/man/use.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2023, 2026 R Core Team
% Distributed under GPL 2 or later

\name{use}
\title{Use Packages}
\alias{use}
\description{
  Use packages in R scripts by loading their namespace and attaching a
  package environment including (a subset of) their exports to the
  search path.
}
\usage{
use(package, include.only, pos = 2)
}
\arguments{
  \item{package}{a character string given the name of a package.}
  \item{include.only}{character vector of names of objects to
    include in the attached environment frame.  If missing, all exports
    are included.}
  \item{pos}{the position in \code{\link{search}()} at which the package
    should be attached.}
}
\details{
  This is a simple wrapper around \code{\link{library}} which always
  uses \code{attach.required = FALSE}, so that packages listed in the
  \code{Depends} clause of the \code{DESCRIPTION} file of the package to
  be used never get attached automatically to the search path.

  This therefore allows to write R scripts with full control over what
  gets found on the search path.  In addition, such scripts can easily
  be integrated as package code, replacing the calls to \code{use} by
  the corresponding \code{ImportFrom} directives in \file{NAMESPACE}
  files.
}
\value{
  (invisibly) a logical indicating whether the package to be used is
  available.
}
\note{
  This functionality is still experimental: interfaces may change in
  future versions.
}
\examples{
\donttest{
## From a posting on R-devel (2026-07-23) -- attach these packages in the exact order:
myPkgs <- c("data.table", "caret", "lattice", "ggplot2")
s0 <- search()
invisible(lapply(rev(myPkgs), use)) ; search()
###              ^^^^^^^^^^^ rev() needed
stopifnot(identical(myPkgs, sub("^package:", '', srch <- search()[2:5])))
invisible(lapply(srch, detach, character.only=TRUE)) # and clean & check:
stopifnot(identical(s0, search()))

## At the very end (at "base" position), now via (new in R 4.7.*)  pos = *
invisible(lapply(myPkgs, use, pos = "package:base"))
##               ^^^^^^  no rev() as we append at very end
nS <- length(print( search() ))
stopifnot(identical(myPkgs, sub("^package:", '', search()[(nS-4):(nS-1)])))
## and clean up again:
invisible(lapply(srch, detach, character.only=TRUE)) ; stopifnot(identical(s0, search()))
}%dont 
}
