\name{capabilities}
\alias{capabilities}
\title{Report Capabilities of this Build of R}
\description{
  Report on the optional features which have been compiled into this
  build of \R.
}
\usage{
capabilities(what = NULL)
}
\arguments{
  \item{what}{character vector or \code{NULL}, specifiying
    required components.}
}
\value{
  A named logical vector. Current components are
    \item{jpeg}{Is the \code{\link{jpeg}} function operational?}
    \item{png}{Is the \code{\link{png}} function operational?}
    \item{tcltk}{Is the \code{tcltk} package operational?}
    \item{X11}{(Unix)  Are \code{X11} and the data editor available?}
    \item{libz}{Is \code{\link{gzfile}} available?}
    \item{libxml}{Are \code{\link{url}} and the internal method for
      \code{\link{download.file}} available?}
}
\seealso{\code{\link{.Platform}}}

\examples{
capabilities()

if(!capabilities("libxml"))
   warning("internal download.file() is not available")

## See also the examples for `connections'.
}
\keyword{utilities}
