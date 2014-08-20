% File src/library/utils/man/packageDescription.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Copyright 2005-2010 The R Foundation
% Distributed under GPL 2 or later

\name{packageDescription}
\alias{packageDescription}
\alias{packageVersion}
\alias{print.packageDescription}
\title{Package Description}
\description{
  Parses and returns the \file{DESCRIPTION} file of a package.
}
\usage{
packageDescription(pkg, lib.loc = NULL, fields = NULL,
                   drop = TRUE, encoding = "")
packageVersion(pkg, lib.loc = NULL)
}
\arguments{
  \item{pkg}{a character string with the package name.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages and namespaces are searched before the libraries.}
  \item{fields}{a character vector giving the tags of fields to return
    (if other fields occur in the file they are ignored).}
  \item{drop}{If \code{TRUE} and the length of \code{fields} is 1, then
    a single character string with the value of the respective field is
    returned instead of an object of class \code{"packageDescription"}.}
  \item{encoding}{If there is an \code{Encoding} field, to what encoding
    should re-encoding be attempted?  If \code{NA}, no re-encoding.  The
    other values are as used by \code{\link{iconv}}, so the default
    \code{""} indicates the encoding of the current locale.}
}
\details{
  A package will not be \sQuote{found} unless it has a \file{DESCRIPTION} file
  which contains a valid \code{Version} field.  Different warnings are
  given when no package directory is found and when there is a suitable
  directory but no valid \file{DESCRIPTION} file.

  An \link{attach}ed environment named to look like a package
  (e.g., \code{package:utils2}) will be ignored.

  \code{packageVersion()} is a convenience shortcut, allowing things
  like \code{if (packageVersion("MASS") < "7.3") { do.things } }.
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

  \code{packageVersion()} returns a (length-one) object of class
  \code{"\link{package_version}"}.
}
\seealso{
  \code{\link{read.dcf}}
}
\examples{\donttest{
packageDescription("stats")
packageDescription("stats", fields = c("Package", "Version"))

packageDescription("stats", fields = "Version")
packageDescription("stats", fields = "Version", drop = FALSE)

if(packageVersion("MASS") < "7.3.29")
  message("you need to update 'MASS'")
}}
\keyword{utilities}
