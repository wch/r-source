% File src/library/utils/man/removeSource.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2014 R Core Team
% Distributed under GPL 2 or later

\name{removeSource}
\alias{removeSource}
\title{Remove Stored Source from a Function}
\description{
  When \code{options("keep.source")} is \code{TRUE}, a copy of the
  original source code to a function is stored with it.  This function
  removes that copy.
}
\usage{
removeSource(fn)
}
\arguments{
  \item{fn}{
    A single function from which to remove the source.
  }
}
\details{
  This removes the \code{"srcref"} and related attributes.
}
\value{
  A copy of the function with the source removed.
}
\seealso{
  \code{\link{srcref}} for a description of source reference records,
  \code{\link{deparse}} for a description of how functions are deparsed.
}
\examples{
fn <- function(x) {
  x + 1 # A comment, kept as part of the source
}
fn
fn <- removeSource(fn)
fn
}
\keyword{ utility }
