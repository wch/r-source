\name{Rwin configuration}
\alias{Rconsole}
\alias{Rdevga}
\title{R for Windows Configuration}
\description{
  The file \code{Rconsole} configures the R GUI console in this
  Windows port. The file \code{Rdevga} configures the graphics devices
  \code{windows}, \code{win.graph}, \code{win.metafile} and \code{win.print}.
}
\value{
  Each of the files contains details in comments of how to set the
  values.

  At the time of writing \code{Rdevga} configured the mapping of font
  numbers to fonts, and \code{Rconsole} configured the appearance
  (single or multiple document interface, toolbar, statusbar on MDI),
  size and colours of the GUI console, and whether resizing the
  console sets \code{\link{options}("width")}.
}
\details{
  There are system copies of these files in \code{R\_HOME\\etc}. Users
  can have personal copies of the files: these are looked for in the
  location given by the environment variable \code{R\_USER}. The system
  files are read only if a corresponding personal file is not found.

  If the environment variable \code{R\_USER} is not set, the \R system
  sets it to \code{HOME} if that is set (stripping any trailing
  slash), otherwise to \code{HOMEDIR:HOMEPATH} is \code{HOMEDIR} is set
  otherwise to the working directory.
}
\author{Guido Masarotto}
\seealso{\code{\link{windows}}}

\keyword{utilities}
