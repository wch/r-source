\name{Rwin configuration}
\alias{Rconsole}
\alias{Rdevga}
\title{R for Windows Configuration}
\description{
  The file \file{Rconsole} configures the R GUI console in this
  Windows port. The file \file{Rdevga} configures the graphics devices
  \code{windows}, \code{win.graph}, \code{win.metafile} and \code{win.print}.
}
\value{
  Each of the files contains details in comments of how to set the
  values.

  At the time of writing \file{Rdevga} configured the mapping of font
  numbers to fonts, and \file{Rconsole} configured the appearance
  (single or multiple document interface, toolbar, statusbar on MDI),
  size, font and colours of the GUI console, and whether resizing the
  console sets \code{\link{options}("width")}.

  The file \file{Rconsole} also configures the internal pager.  This
  shares the font and colours of the console, but can be sized
  separately.
}
\details{
  There are system copies of these files in \file{R\_HOME\\etc}. Users
  can have personal copies of the files: these are looked for in the
  location given by the environment variable \code{R_USER}. The system
  files are read only if a corresponding personal file is not found.

  If the environment variable \code{R_USER} is not set, the \R system
  sets it to \code{HOME} if that is set (stripping any trailing
  slash), otherwise to \code{HOMEDIR:HOMEPATH} is \code{HOMEDIR} is set
  otherwise to the working directory.
}
\note{
  The \code{GUI preferences} item on the \code{Edit} menu brings up an
  form which can be used to edit the console settings, and to save them
  to a file.
}
\author{Guido Masarotto}
\seealso{\code{\link{windows}}}

\keyword{utilities}
