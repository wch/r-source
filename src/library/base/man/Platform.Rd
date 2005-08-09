\name{.Platform}
\title{Platform Specific Variables}
\usage{
.Platform
}
\alias{.Platform}
\description{
  \code{.Platform} is a list with some details of the platform under
  which \R was built.  This provides means to write OS portable \R
  code.
}
\value{
  A list with at least the following components:
  
  \item{OS.type}{
    character, giving the \bold{O}perating \bold{S}ystem
    (family) of the computer.  One of \code{"unix"} or \code{"windows"}.
  }
  \item{file.sep}{
    character, giving the \bold{file} \bold{sep}arator,
    used on your platform, e.g., \code{"/"} on Unix alikes.
  }
  \item{dynlib.ext}{
    character, giving the file name \bold{ext}ension of
    \bold{dyn}amically loadable \bold{lib}raries, e.g., \code{".dll"} on
    Windows.
  }
  \item{GUI}{
    character, giving the type of GUI in use, or
    \code{"unknown"} if no GUI can be assumed.
  }
  \item{endian}{
    character, \code{"big"} or \code{"little"}, giving the
    endianness of the processor in use.
  }
  \item{pkgType}{
    character, the preferred setting for \code{\link{options}("pkgType")}.
    Values \code{"source"}, \code{"mac.binary"} and \code{"win.binary"}
    are currently in use.
  }
  \item{path.sep}{
    character, giving the \bold{path} \bold{sep}arator,
    used on your platform, e.g., \code{":"} on Unix alikes and
    \code{";"} on Windows.  Used to separate paths in variables
    such as \code{PATH} and \code{TEXINPUTS}.
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
