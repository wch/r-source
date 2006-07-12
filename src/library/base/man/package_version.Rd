\name{package-version}		% Cannot have 'package_version' ...
\alias{package_version}
\alias{is.package_version}
\alias{as.package_version}
\alias{as.character.package_version}
\alias{print.package_version}
\alias{Ops.package_version}
\alias{Summary.package_version}
\alias{c.package_version}
\alias{[.package_version}
\alias{[[.package_version}
\alias{$.package_version}
\alias{as.data.frame.package_version}
\alias{.encode_package_version}
\alias{.decode_package_version}
\alias{getRversion}
\title{Package versions}
\description{A simple S3 class for representing package versions, and
  associated methods.}
\usage{
package_version(x, strict = TRUE)

getRversion()
}
\arguments{
  \item{x}{a character vector with package version strings, or an R
    version object as obtained by \code{\link{R.version}}.}
  \item{strict}{a logical indicating whether invalid package versions
    should results in an error (default) or not.}
}
\details{
  \R (package) versions are sequences of at least two non-negative
  integers, usually (e.g., in package \file{DESCRIPTION} files)
  represented as character strings with the elements of the sequence
  concatenated and separated by single \samp{.} or \samp{-} characters.

  \code{package_version} creates a representation from such strings
  which allows for coercion and testing, combination, comparison,
  summaries (min/max), inclusion in data frames, subscripting, and
  printing.

  \code{getRversion} returns the version of the running \R as an object
  of class \code{"package_version"}.
}
\seealso{
  \code{\link{compareVersion}}
}
\examples{
x <- package_version(c("1.2-4", "1.2-3", "2.1"))
x < "1.4-2.3"
c(min(x), max(x))
x[2, 2]
x$major
x$minor
}
\keyword{utilities}
