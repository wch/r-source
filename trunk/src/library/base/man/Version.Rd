\name{R.Version}
\title{Version Information}
\usage{
R.Version()
R.version
R.version.string
}
\alias{R.Version}
\alias{R.version}
\alias{version}
\alias{R.version.string}
\description{
    \code{R.Version()} provides detailed information about the version of
    \R running. \cr
    \code{R.version} is a variable (a \code{\link{list}}) holding this
    information (and \code{version} is a copy of it for
    S compatibility).
}
\value{
 \code{R.Version} returns a list with character-string components
 \item{platform}{the platform for which \R was built.  A triplet of the
   form CPU-VENDOR-OS, as determined by the configure script.  E.g,
   \code{"i586-unknown-linux"} or \code{"i386-pc-mingw32"}.
 }
  \item{arch}{the architecture (CPU) \R was built on/for.}
  \item{os}{the underlying operating system}
  \item{system}{CPU and OS, separated by a comma.}
  \item{status}{the status of the version (e.g., \code{"Alpha"})}
  \item{major}{the major version number}
  \item{minor}{the minor version number, including the patchlevel}
  \item{year}{the year the version was released}
  \item{month}{the month the version was released}
  \item{day}{the day the version was released}
  \item{svn rev}{the Subversion revision number, which should be either
    \code{"unknown"} or a single number. (A range of numbers or a number
    with \code{M} or \code{S} appended indicates inconsistencies in the
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
\seealso{
  \code{\link{getRversion}},
  \code{\link{.Platform}}
}
\examples{
R.version$os # to check how lucky you are ...
plot(0) # any plot
mtext(R.version.string, side=1,line=4,adj=1)# a useful bottom-right note
}
\keyword{environment}
\keyword{sysdata}
\keyword{programming}
