\name{Rwin configuration}
\alias{Rconsole}
\alias{Rdevga}
\concept{console}
\concept{fonts}
\concept{preferences}
\title{R for Windows Configuration}
\description{
  The file \file{Rconsole} configures the R GUI console in this
  Windows port. The file \file{Rdevga} configures the graphics devices
  \code{windows}, \code{win.graph}, \code{win.metafile} and \code{win.print}.
}
\value{
  Each of the files contains details in its comments of how to set the
  values.

  At the time of writing \file{Rdevga} configured the mapping of font
  numbers to fonts, and \file{Rconsole} configured the appearance
  (single or multiple document interface, toolbar, statusbar on MDI),
  size, font and colours of the GUI console, and whether resizing the
  console sets \code{\link{options}("width")}.

  The file \file{Rconsole} also configures the internal pager.  This
  shares the font and colours of the console, but can be sized
  separately.

  \file{Rconsole} can also set the initial positions of the console and
  the graphics device, as well as the size and position of the MDI
  workspace in MDI mode.
}
\details{
  There are system copies of these files in \file{R\_HOME\\etc}.  Users
  can have personal copies of the files: these are looked for in the
  location given by the environment variable \env{R\_USER}. The system
  files are read only if a corresponding personal file is not found.

  If the environment variable \env{R\_USER} is not set, the \R system
  sets it to \env{HOME} if that is set (stripping any trailing slash),
  otherwise to the Windows ``persona''  directory, 
  otherwise to \code{{HOMEDRIVE}{HOMEPATH}} if \code{HOMEDRIVE} and
  \code{HOMEDRIVE} are both set
  otherwise to the working directory.  This is as described in the file
  \file{rw-FAQ}.
}
\section{Chinese/Japanese/Kprean}{
  Users of these languages will need to select a suitable font for the
  console (perhaps \code{FixedSys} or \code{MS Mincho}) and for the
  graphics device (\code{Arial} can be replaced by \code{Arial Unicode MS}).

  Which fonts are available depends on the version of Windows.
}
\note{
  The \code{GUI preferences} item on the \code{Edit} menu brings up an
  form which can be used to edit the console settings, and to save them
  to a file.
}
\author{Guido Masarotto}
\seealso{\code{\link{windows}}}
\examples{
ruser <- Sys.getenv("R_USER")
cat("\n\nLocation for personal configuration files is\n   R_USER = ",
    ruser, "\n\n", sep="")
## see if there are personal configuration files
sapply(c("Rconsole", "Rdevga"), function(x) file.exists(file.path(ruser, x)))

## show the configuration files used
showConfig <- function(file)
{
    ruser <- Sys.getenv("R_USER")
    path <- file.path(ruser, file)
    if(!file.exists(path)) path <- file.path(R.home(), "etc", file)
    file.show(path, header=path)
}
showConfig("Rconsole")
}
\keyword{utilities}
