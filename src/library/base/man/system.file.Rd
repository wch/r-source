\name{system.file}
\title{Find Names of R System Files}
\usage{
system.file(..., pkg = .packages(), lib = .lib.loc)
}
\alias{system.file}
\arguments{
  \item{...}{character strings, specifying subdirectory and file(s)
      within some package.
      The default, none or \code{""}, returns the root of
      package(s). Wildcards are not supported.}
  \item{pkg}{a character vector with package names.}
  \item{lib}{a character vector with path names of \R package
      libraries, see \code{\link{.lib.loc}} for the default.}
}
\description{
    Finds the full file names of files in packages etc.
}
\value{
    A character vector of positive length,
    containing the file names that matched \code{...}, or the empty
    string, \code{""}, if none matched. If matching the root of a
    package, there is no trailing separator.

    As a special case, \code{system.file()} gives the root of the
    \code{base} package only.
}
\seealso{\code{\link{list.files}}}
\examples{
system.file()            # The root of the `base' package
system.file(pkg="lqs")   # The root of package `lqs'
system.file("INDEX")
system.file("help/AnIndex", pkg = c("stepfun", "mva"))
}
\keyword{file}
\keyword{utilities}
