\name{Platform}
\title{Platform Specific Variables}
\usage{
.Platform
Platform()
}
\alias{.Platform}
\alias{Platform}
\description{
  \code{.Platform} is a list with functions and variables as
  components.  This provides means to write OS portable \R
  code. 
}
\details{
  Currently, \code{.Platform <- Platform()} when \R starts up. 
}
\value{
  \code{.Platform} is list with at least the following components:
  \item{OS.type}{character, giving the \bold{O}perating \bold{S}ystem
    (family) of the computer.  One of the following values is returned:
  \code{"unix"},  \code{"mac"}, or  \code{"windows"} (in historical order).}
  \item{file.sep}{character, giving the \bold{file} \bold{sep}arator,
    used on your platform, e.g., \code{"/"} on Unix alikes.
  }
  \item{dynlib.ext}{character, giving the file name \bold{ext}ension of
    \bold{dyn}amically loadable \bold{lib}raries, e.g., \code{".dll"} on
    Windows.
  }
  \item{GUI}{character, giving the type of GUI in use, or
    \code{"unknown"} if no GUI can be assumed.
  }
}
\seealso{\code{\link{system}} for invoking platform-specific system commands.}
\examples{
if(.Platform$OS.type == "unix") {
   system.test <- function(...) { system(paste("test", ...)) == 0 }
   dir.exists <- function(dir) sapply(dir, function(d)system.test("-d", d))
   dir.exists(c(R.home(), "/tmp", "~", "/NO"))# > T T T F
}
}
\keyword{file}
\keyword{utilities}
