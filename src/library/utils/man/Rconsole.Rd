% File src/library/utils/man/Rconsole.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{Rwin configuration}
\title{R for Windows Configuration}
\Rdversion{1.1}
\alias{Rconsole}
\alias{Rdevga}
\alias{loadRconsole}
\concept{console}
\concept{fonts}
\concept{preferences}
\description{
  The file \file{Rconsole} configures the R GUI (\command{Rgui}) console
  under MS Windows and \code{loadRconsole(*)} loads a new configuration.

  The file \file{Rdevga} configures the graphics devices
  \code{\link{windows}}, \code{win.graph}, \code{win.metafile} and
  \code{win.print}, as well as the bitmap devices \code{\link{bmp}},
  \code{jpeg}, \code{png} and \code{tiff} (which use for \code{type =
  "windows"} use \code{windows} internally).
}
\usage{
  loadRconsole(file)
}
\arguments{
  \item{file}{The file from which to load a new \file{Rconsole} configuration.
   By default a file dialog is used to select a file. }
}
\value{
  Each of the files contains details in its comments of how to set the
  values.

  At the time of writing \file{Rdevga} configured the mapping of font
  numbers to fonts, and \file{Rconsole} configured the appearance
  (single or multiple document interface, toolbar, status bar on MDI),
  size, font and colours of the GUI console, and whether resizing the
  console sets \code{\link{options}("width")}.

  The file \file{Rconsole} also configures the internal pager.  This
  shares the font and colours of the console, but can be sized
  separately.

  \file{Rconsole} can also set the initial positions of the console and
  the graphics device, as well as the size and position of the MDI
  workspace in MDI mode.

  \code{loadRconsole} is called for its side effect of loading new
  defaults.  It returns no useful value.
}

\details{
  There are system copies of these files in
  \file{\var{\link{R_HOME}}\\etc}.  Users can have personal copies of
  the files: these are looked for in the location given by the
  environment variable \env{R_USER}. The system files are read only if a
  corresponding personal file is not found.

  If the environment variable \env{R_USER} is not set, the \R system
  sets it to \env{HOME} if that is set (stripping any trailing slash),
  otherwise to the Windows \sQuote{personal}  directory,
  otherwise to \code{{HOMEDRIVE}{HOMEPATH}} if \code{HOMEDRIVE} and
  \code{HOMEDRIVE} are both set
  otherwise to the working directory.  This is as described in the file
  \file{rw-FAQ}.
}
\section{Chinese/Japanese/Korean}{
  Users of these languages will need to select a suitable font for the
  console (perhaps \code{MS Mincho}) and for the graphics device
  (although the default \code{Arial} has many East Asian characters).
  It is essential that the font selected for the console has
  double-width East Asian characters -- many monospaced fonts do not.
}
\note{
  The \code{GUI preferences} item on the \code{Edit} menu brings up an
  dialog box which can be used to edit the console settings, and to save them
  to a file.

  This is only available on Windows.
}
\author{
  Guido Masarotto and R-core members
}
\seealso{\code{\link{windows}}}
\examples{
if(.Platform$OS.type == "windows") withAutoprint({
  ruser <- Sys.getenv("R_USER")
  cat("\n\nLocation for personal configuration files is\n   R_USER = ",
      ruser, "\n\n", sep = "")
  ## see if there are personal configuration files
  file.exists(file.path(ruser, c("Rconsole", "Rdevga")))

  ## show the configuration files used
  showConfig <- function(file)
  {
      ruser <- Sys.getenv("R_USER")
      path <- file.path(ruser, file)
      if(!file.exists(path)) path <- file.path(R.home(), "etc", file)
      file.show(path, header = path)
  }
  showConfig("Rconsole")
})
}
\keyword{utilities}
