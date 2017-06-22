% File src/library/base/man/Platform.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{.Platform}
\title{Platform Specific Variables}
\usage{
.Platform
}
\alias{.Platform}
\description{
  \code{.Platform} is a list with some details of the platform under
  which \R was built.  This provides means to write OS-portable \R
  code.
}
\value{
  A list with at least the following components:

  \item{OS.type}{
    character string, giving the \bold{O}perating \bold{S}ystem
    (family) of the computer.  One of \code{"unix"} or \code{"windows"}.
  }
  \item{file.sep}{
    character string, giving the \bold{file} \bold{sep}arator used on your
    platform: \code{"/"} on both Unix-alikes \emph{and} on Windows (but
    not on the former port to Classic Mac OS).
  }
  \item{dynlib.ext}{
    character string, giving the file name \bold{ext}ension of
    \bold{dyn}amically loadable \bold{lib}raries, e.g., \code{".dll"} on
    Windows and \code{".so"} or \code{".sl"} on Unix-alikes.  (Note for
    macOS users: these are shared objects as loaded by
    \code{\link{dyn.load}} and not dylibs: see \code{\link{dyn.load}}.)
  }
  \item{GUI}{
    character string, giving the type of GUI in use, or \code{"unknown"}
    if no GUI can be assumed.  Possible values are for Unix-alikes the
    values given via the \option{-g} command-line flag (\code{"X11"},
    \code{"Tk"}), \code{"AQUA"} (running under \code{R.app} on macOS),
    \code{"Rgui"} and \code{"RTerm"} (Windows) and perhaps others under
    alternative front-ends or embedded \R.
  }
  \item{endian}{
    character string, \code{"big"} or \code{"little"}, giving the
    \sQuote{endianness} of the processor in use.  This is relevant when it is
    necessary to know the order to read/write bytes of e.g.\sspace{}an
    integer or double from/to a \link{connection}: see
    \code{\link{readBin}}.
  }
  \item{pkgType}{
    character string, the preferred setting for
    \code{\link{options}("pkgType")}.  Values \code{"source"},
    \code{"mac.binary.el-capitan"} and \code{"win.binary"} are currently
    in use.

    This should \strong{not} be used to identify the OS.
  }
  \item{path.sep}{
    character string, giving the \bold{path} \bold{sep}arator,
    used on your platform, e.g., \code{":"} on Unix-alikes and
    \code{";"} on Windows.  Used to separate paths in environment
    variables such as \code{PATH} and \code{TEXINPUTS}.
  }
  \item{r_arch}{
    character string, possibly \code{""}.  The name of an
    architecture-specific directory used in this build of \R.
  }
}

\section{AQUA}{
  \code{.Platform$GUI} is set to \code{"AQUA"} under the macOS GUI,
  \code{R.app}. This has a number of consequences:
  \itemize{
%%     \item the \env{DISPLAY} environment variable is set to \code{":0"}
%%     if unset. (However, it will be set on a machine which has
%%     XQuartz installed.)
    \item \file{/usr/local/bin} is \emph{appended} to the \env{PATH}
    environment variable.
    \item the default graphics device is set to \code{quartz}.
    \item selects native (rather than Tk) widgets for the \code{graphics
      = TRUE} options of \code{\link{menu}} and \code{\link{select.list}}.
    \item HTML help is displayed in the internal browser.
    \item the spreadsheet-like data editor/viewer uses a Quartz version
    rather than the X11 one.
  }
}
\seealso{
  \code{\link{R.version}} and \code{\link{Sys.info}} give more details
  about the OS.  In particular, \code{R.version$platform} is the
  canonical name of the platform under which \R was compiled.

  \code{\link{.Machine}} for details of the arithmetic used, and
  \code{\link{system}} for invoking platform-specific system commands.

  \code{\link{capabilities}} and \code{\link{extSoftVersion}} (and links
  there) for availability of capabilities partly \emph{external} to \R
  but used from \R functions.
}

\examples{
## Note: this can be done in a system-independent way by dir.exists()
if(.Platform$OS.type == "unix") {
   system.test <- function(...) system(paste("test", ...)) == 0L
   dir.exists2 <- function(dir)
       sapply(dir, function(d) system.test("-d", d))
   dir.exists2(c(R.home(), "/tmp", "~", "/NO")) # > T T T F
}
}
\keyword{file}
\keyword{utilities}
