\name{capabilities}
\alias{capabilities}
\title{Report Capabilities of this Build of R}
\description{
  Report on the optional features which have been compiled into this
  build of \R.
}
\usage{
capabilities()
}
\value{
  A named logical vector. Current components are
  \describe{
    \item{jpeg}{Is the \code{\link{jpeg}} function operational?}
    \item{png}{Is the \code{\link{png}} function operational?}
    \item{tcltk}{Is the \code{tcltk} package operational?}
    \item{X11}{(Unix)  Are \code{X11} and the data editor available?}
    \item{libz}{Is \code{\link{gzfile}} available?}
    \item{libxml}{Are \code{\link{url}} and the internal method for
      \code{\link{download.file}} available?}
  }
}
\seealso{\code{\link{.Platform}}}

\examples{
capbilities()
}
\keyword{utilities}
