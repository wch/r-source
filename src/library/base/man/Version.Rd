% File src/library/base/man/Version.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{R.Version}
\title{Version Information}
\alias{R.Version}
\alias{R.version}
\alias{version}
\alias{R.version.string}
\usage{
R.Version()
R.version
R.version.string
version
}
\description{
  \code{R.Version()} provides detailed information about the version of
  \R running.

  \code{R.version} is a variable (a \code{\link{list}}) holding this
  information (and \code{version} is a copy of it for S compatibility).
}
\value{
 \code{R.Version} returns a list with character-string components
 \item{platform}{the platform for which \R was built.  A triplet of the
   form CPU-VENDOR-OS, as determined by the configure script.  E.g,
   \code{"i686-unknown-linux-gnu"} or \code{"i386-pc-mingw32"}.
 }
  \item{arch}{the architecture (CPU) \R was built on/for.}
  \item{os}{the underlying operating system.}
  \item{system}{CPU and OS, separated by a comma.}
  \item{status}{the status of the version (e.g., \code{"alpha"})}
  \item{major}{the major version number}
  \item{minor}{the minor version number, including the patchlevel}
  \item{year}{the year the version was released}
  \item{month}{the month the version was released}
  \item{day}{the day the version was released}
  \item{svn rev}{the Subversion revision number, which should be either
    \code{"unknown"} or a single number. (A range of numbers or a number
    with \samp{M} or \samp{S} appended indicates inconsistencies in the
    sources used to build this version of \R.)
  }
  \item{language}{always \code{"R"}.}
  \item{version.string}{a
    \code{\link{character}} string concatenating some of the info above,
    useful for plotting, etc.}

  \code{R.version} and \code{version} are lists of class
  \code{"simple.list"} which has a \code{print} method.
}
\note{
  Do \emph{not} use \code{R.version$os} to test the platform the
  code is running on: use \code{.Platform$OS.type} instead.  Slightly
  different versions of the OS may report different values of
  \code{R.version$os}, as may different versions of \R.

  \code{R.version.string} is a copy of \code{R.version$version.string}
  for simplicity and backwards compatibility.
}
\details{
  This gives details of the OS under which \R was built, not the one
  under which it is currently running (for which see
  \code{\link{Sys.info}}).

  Note that OS names might not be what you expect: for example macOS
  Mavericks 10.9.4 identifies itself as \samp{darwin13.3.0}, Linux
  usually as \samp{linux-gnu} and Solaris 10 as \samp{solaris2.10}.
}
\seealso{
  \code{\link{sessionInfo}} which provides additional information;
  \code{\link{getRversion}} typically used inside R code,
  \code{\link{.Platform}}, \code{\link{Sys.info}}.
}
\examples{
require(graphics)

R.version$os # to check how lucky you are ...
plot(0) # any plot
mtext(R.version.string, side = 1, line = 4, adj = 1) # a useful bottom-right note

## a good way to detect macOS:
if(grepl("^darwin", R.version$os)) message("running on macOS")
}
\keyword{environment}
\keyword{sysdata}
\keyword{programming}
