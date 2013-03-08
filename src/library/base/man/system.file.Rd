% File src/library/base/man/system.file.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{system.file}
\title{Find Names of R System Files}
\usage{
system.file(\dots, package = "base", lib.loc = NULL,
            mustWork = FALSE)
}
\alias{system.file}
\arguments{
  \item{\dots}{character vectors, specifying subdirectory and file(s)
    within some package.  The default, none, returns the
    root of the package.  Wildcards are not supported.}
  \item{package}{a character string with the name of a single package.
    An error occurs if more than one package name is given.}
  \item{lib.loc}{a character vector with path names of \R libraries.
    See \sQuote{Details} for the meaning of the default value of \code{NULL}.}
  \item{mustWork}{logical.  If \code{TRUE}, an error is given if there
    are no matching files.}
}
\description{
  Finds the full file names of files in packages etc.
}
\details{
  This checks the existence of the specified files with
  \code{\link{file.exists}}.  So file paths are only returned if there
  are sufficient permissions to establish their existence.

  The unnamed arguments in \code{...} are usually character strings, but
  if character vectors they are recycled to the same length.

  This uses \code{\link{find.package}} to find the package, and hence
  with the default \code{lib.loc = NULL} looks first for attached
  packages then in each library listed in \code{\link{.libPaths}()}.
  Note that if a namespace is loaded but the package is not attached,
  this will look only on \code{\link{.libPaths}()}.
}
\value{
  A character vector of positive length, containing the file paths
  that matched \code{\dots}, or the empty string, \code{""}, if none
  matched (unless \code{mustWork = TRUE}).

  If matching the root of a package, there is no trailing separator.

  \code{system.file()} with no arguments gives the root of the
  \pkg{base} package.
}
\seealso{
  \code{\link{R.home}} for the root directory of the \R
  installation, \code{\link{list.files}}.

  \code{\link{Sys.glob}} to find paths via wildcards.
}
\examples{
system.file()                  # The root of the 'base' package
system.file(package = "stats") # The root of package 'stats'
system.file("INDEX")
system.file("help", "AnIndex", package = "splines")
}
\keyword{file}
\keyword{utilities}
