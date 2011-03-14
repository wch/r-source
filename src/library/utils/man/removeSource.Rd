\name{removeSource}
\alias{removeSource}
\title{
Remove stored source from a function.
}
\description{
When \code{options("keep.source")} is \code{TRUE}, 
a copy of the original source code to a function is stored with it.  This function
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
This removes both the \code{"source"} attribute (from \R version 2.13.x or earlier)
and the \code{"srcref"} and related attributes.
}
\value{
A copy of the function with the source removed.
}
\seealso{
\code{\link{srcref}} for a description of source reference records, \code{\link{deparse}}
for a description of how functions are deparsed.
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
