\name{macintosh}
\alias{macintosh}
\title{Macintosh Quickdraw Graphics Device}
\description{
  This command starts the graphics device driver for the Macintosh.
}
\usage{
macintosh(width = 5, height = 5, pointsize = 12)
}
\arguments{
  \item{width}{the (nominal) width of the plotting window in inches.}
  \item{height}{the (nominal) height of the plotting window in inches.}
  \item{pointsize}{the default pointsize of plotted text.}
}
\details{
  Selecting \code{Print} from the file menu when the graphics
  window is active will spool the graph to the printer.
  However, it is usually more effective to use \code{Copy} to copy
  the graph to the scrap for pasting directly into
  a Macintosh application such as \emph{WriteNow} or \emph{Word}.
}
\seealso{
  \code{\link{postscript}}, \code{\link{device}}.
}
\keyword{device}
