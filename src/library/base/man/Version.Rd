\name{Version}
\title{Version Information}
\usage{
Version()
version
}
\alias{Version}
\alias{version}
\description{
  \code{Version()} provides detailed information about the version of R running.

  \code{version} is a variable holding this information.
}
\value{
 \code{Version} returns a list with components
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
  \item{month}{the year the version was released}
  \item{day}{the year the version was released}
  \item{language}{always \code{"R"}.}
}
\seealso{
\code{\link{machine}}.
}
\examples{
version$os # to check how lucky you are ...
}
\keyword{environment}
\keyword{sysdata}
\keyword{programming}
