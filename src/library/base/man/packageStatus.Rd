\name{packageStatus}
\title{Package Management Tools}
\alias{compareVersion}
\alias{newestVersion}
\alias{packageStatus}
\alias{print.packageStatus}
\alias{summary.packageStatus}
\alias{update.packageStatus}
\alias{upgrade.packageStatus}
\alias{upgrade}
\description{Summarize information about installed packages and packages
  available at various repositories, and automatically upgrade outdated
  packages. These tools will replace \code{\link{update.packages}} and friends
  in the future and are currently work in progress.}
\usage{
packageStatus(lib.loc = .lib.loc, repositories = getOption("repositories"))
\method{summary}{packageStatus}(object, ...)
\method{update}{packageStatus}(object, lib.loc=levels(object$inst$LibPath),
       repositories=levels(object$avail$Repository), ...)
\method{upgrade}{packageStatus}(object, ask=TRUE, ...)
}
\examples{
\dontrun{
x <- packageStatus()
print(x)
summary(x)
upgrade(x)
x <- update(x)
print(x)
}}
\keyword{utilities}
