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
  \item{what}{character vector or \code{NULL}, specifying
    required components.  \code{NULL} implies that all are required.}
}
\value{
  A named logical vector. Current components are
    \item{jpeg}{Is the \code{\link{jpeg}} function operational?}
    \item{png}{Is the \code{\link{png}} function operational?}
    \item{tcltk}{Is the \pkg{tcltk} package operational?}
    \item{X11}{(Unix)  Are the \code{X11} graphics device and the
      X11-based data editor available?}
    \item{http/ftp}{Are \code{\link{url}} and the internal method for
      \code{\link{download.file}} available?}
    \item{sockets}{Are \code{\link{make.socket}} and related functions
      available?}
    \item{libxml}{Is there support for integrating \code{libxml} with
      the \R event loop?}
    \item{fifo}{are FIFO connections supported?}
    \item{cledit}{Is command-line editing available in the current \R
      session?  This is false in non-interactive sessions.
#ifdef unix
      It will be true if \code{readline} support has been compiled in
      and \option{--no-readline} was \emph{not} invoked.
#endif
      }
    \item{IEEE754}{Does this platform have IEEE 754 arithmetic?  Note
      that this is more correctly known by the international standard
      IEC 60559, and will always be true from \R version 2.0.0.  It is
      now deprecated and will be removed in due course.}
}
\seealso{\code{\link{.Platform}}}

\examples{
capabilities()

if(!capabilities("http/ftp"))
   warning("internal download.file() is not available")

## See also the examples for 'connections'.
}
\keyword{utilities}
