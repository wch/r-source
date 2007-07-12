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
    not on the now abandoned port to Classic MacOS).
  }
  \item{dynlib.ext}{
    character string, giving the file name \bold{ext}ension of
    \bold{dyn}amically loadable \bold{lib}raries, e.g., \code{".dll"} on
    Windows and \code{".so"} or \code{".sl"} on Unix-alikes.  (Note for
    MacOS X users: these are shared objects as loaded by
    \code{\link{dyn.load}} and not dylibs.)
  }
  \item{GUI}{
    character string, giving the type of GUI in use, or \code{"unknown"}
    if no GUI can be assumed.  Possible values are for Unix-alikes the
    values given via the \option{-g} command-line flag (\code{"X11"},
    \code{"Tk"}, \code{"none"} and perhaps others under alternative
    front-ends or embedded \R), \code{"AQUA"} (running under
    \code{R.app} on MacOS X), \code{"RGui"} and \code{"RTerm"} (Windows).
  }
  \item{endian}{
    character string, \code{"big"} or \code{"little"}, giving the
    endianness of the processor in use.  This is relevant when it is
    necessary to know the order to read/write bytes of e.g. an integer or
    double from/to a connection: see \code{\link{readBin}}. 
  }
  \item{pkgType}{
    character string, the preferred setting for
    \code{\link{options}("pkgType")}. Values \code{"source"},
    \code{"mac.binary"} and \code{"win.binary"} are currently in use.
  }
  \item{path.sep}{
    character string, giving the \bold{path} \bold{sep}arator,
    used on your platform, e.g., \code{":"} on Unix-alikes and
    \code{";"} on Windows.  Used to separate paths in variables
    such as \code{PATH} and \code{TEXINPUTS}.
  }
  \item{r_arch}{
    character string, possibly \code{""}.  The name of the
    architecture-specific directories used in this build of \R.
  }
}
\seealso{
  \code{\link{R.version}} and \code{\link{Sys.info}} give more details
  about the OS.  In particular, \code{R.version$platform} is the
  canonical name of the platform under which \R was compiled.
  
  \code{\link{.Machine}} for details of the arithmetic used, and
  \code{\link{system}} for invoking platform-specific system commands.
}

\examples{
## Note: this can be done in a system-independent way by file.info()$isdir
if(.Platform$OS.type == "unix") {
   system.test <- function(...) { system(paste("test", ...)) == 0 }
   dir.exists <- function(dir) sapply(dir, function(d)system.test("-d", d))
   dir.exists(c(R.home(), "/tmp", "~", "/NO"))# > T T T F
}
}
\keyword{file}
\keyword{utilities}
