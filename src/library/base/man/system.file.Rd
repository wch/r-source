\name{system.file}
\title{Find Names of R System Files}
\usage{
system.file(file = "", pkg = .packages(), lib = .lib.loc)
}
\alias{system.file}
\arguments{
  \item{file}{a character string, specifying subdirectory and file(s).
    The default, \code{""} returns the root of the base package.}
  \item{pkg}{a character vector with package names.}
  \item{lib}{a character vector with path names of \R{} package
    libraries, see \code{\link{.lib.loc}} for the default.}
}
\description{
  \code{system.file} returns a character vector of positive length,
  containing the file names that matched \code{file}, or the empty
  string, \code{""}, if none matched.
}
\examples{
system.file("")            # The root of the `base' package
system.file("INDEX")
system.file("demo/*")      # nothing
system.file("help/AnIndex", pkg = c("stepfun", "mva"))
system.file("data/w*")
}
\keyword{file}
\keyword{utilities}
