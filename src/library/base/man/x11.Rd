\name{x11}
\title{X Window System Graphics}
\usage{
x11(display = "", width = 7, height = 7, ps = 12,
    printcmd = options("printcmd")$printcmd,
    paper = options("papersize")$papersize,
    orientation = "flexible")
print.plot()
save.plot(file)
}
\alias{x11}
\alias{print.plot}
\alias{save.plot}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \code{DISPLAY}.}
  \item{width}{the width of the plotting window in inches.}
  \item{height}{the height of the plotting window in inches.}
  \item{ps}{the default pointsize to be used.}
  \item{printcmd}{the command used to print a hardcopy.}
  \item{paper}{the paper type for hardcopy, currently \code{"a4"} or
    \code{"letter"}.}
  \item{orientation}{the orientation for hardcopy, one of
    \code{"portrait"}, \code{"landscape"}, or \code{"flexible"}.}
}
\description{
  \code{x11} starts a graphics device driver for the X Window System
  (version 11).  This should be done automagically at start up time
  and can only be done on machines that run X.

  \code{print.plot()} prints the plot using the \code{printcmd}
  command.  \code{save.plot("fig.ps")} saves an encapsulated postscript
  version of the plot in the file "fig.ps" (attempting to be pretty
  wysiwyg).  Both make use of the \code{paper} and \code{orientation}
  arguments.
}
\seealso{
  \code{\link{postscript}}, \code{\link{macintosh}},
  \code{\link{Devices}}.
}
\keyword{device}
