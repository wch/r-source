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
    S compatibility), whereas \code{R.version.string} is a simple
    \code{\link{character}} string, useful for plotting, etc.
}
\value{
 \code{R.Version} returns a list with components
  \item{platform}{the platform for which \R was built.  Under Unix, a
    triplet of the form CPU-VENDOR-OS, as determined by the configure
    script.  E.g, \code{"i586-unknown-linux"}.}
  \item{arch}{the architecture (CPU) \R was built on/for.}
  \item{os}{the underlying operating system}
  \item{system}{CPU and OS.}
  \item{status}{the status of the version (e.g., \code{"Alpha"})}
  \item{status.rev}{the status revision level}
  \item{major}{the major version number}
  \item{minor}{the minor version number}
  \item{year}{the year the version was released}
  \item{month}{the month the version was released}
  \item{day}{the day the version was released}
  \item{language}{always \code{"R"}.}
}
\note{
  Do \emph{not} use \code{R.version$os} to test the platform the
  code is running on: use \code{.Platform$OS.type} instead.  Slightly
  different versions of the OS may report different values of
  \code{R.version$os}, as may different versions of \R.
}
\seealso{
  \code{\link{getRversion}}
  \code{\link{.Platform}}.
}
\examples{
R.version$os # to check how lucky you are ...
plot(0) # any plot
mtext(R.version.string, side=1,line=4,adj=1)# a useful bottom-right note
}
\keyword{environment}
\keyword{sysdata}
\keyword{programming}
