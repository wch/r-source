\name{macintosh}
\title{Macintosh Quickdraw Graphics}
\usage{
macintosh(width = 5, height = 5, pointsize = 12)
}
\alias{macintosh}
\description{
This command starts the graphics device driver for the Macintosh.
This is usually done automagically at start up time
and can only be done on a Macintosh.

Selecting \code{Print} from the file menu when the graphics
window is active will spool the graph to the printer,
however,
it is usually more effective to use \code{Copy} to copy
the graph to the scrap for pasting directly into
a Macintosh application such as \emph{WriteNow} or \emph{Word}.
}
\seealso{
\code{\link{postscript}}, \code{\link{x11}}, \code{\link{device}}.
}
\keyword{device}
