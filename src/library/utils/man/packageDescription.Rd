\name{packageDescription}
\alias{packageDescription}
\alias{print.packageDescription}
\title{Package Description}
\description{
  Parses and returns the \file{DESCRIPTION} file of a package.
}
\usage{
packageDescription(pkg, lib.loc = NULL, fields = NULL, drop = TRUE)
}
\arguments{
  \item{pkg}{a character string with the package name.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
  \item{fields}{a character vector giving the tags of fields to return
    (if other fields occur in the file they are ignored).}
  \item{drop}{If \code{TRUE} and the length of \code{fields} is 1, then
    a single character string with the value of the respective field is
    returned instead of an object of class \code{"packageDescription"}.}
}
\details{
  A package will not be \sQuote{found} unless it has a \file{DESCRIPTION} file
  which contains a valid \code{Version} field.  Different warnings are
  given when no package directory is found and when there is a suitable
  directory but no valid \file{DESCRIPTION} file.
}
\value{
  If a \file{DESCRIPTION} file for the given package is found and can
  successfully be read, \code{packageDescription} returns an object of
  class \code{"packageDescription"}, which is a named list with the
  values of the (given) fields as elements and the tags as names, unless
  \code{drop = TRUE}.

  If parsing the \file{DESCRIPTION} file was not successful, it returns
  a named list of \code{NA}s with the field tags as names if \code{fields}
  is not null, and \code{NA} otherwise.
}
\seealso{
  \code{\link{read.dcf}}
}
\examples{
packageDescription("stats")
packageDescription("stats", fields = c("Package", "Version"))

packageDescription("stats", fields = "Version")
packageDescription("stats", fields = "Version", drop = FALSE)
}
\keyword{utilities}
