\name{update.packages}
\alias{update.packages}
\alias{installed.packages}
\alias{CRAN.packages}
\alias{old.packages}
\alias{download.packages}
\alias{install.packages}
\alias{contrib.url}
\title{Download Packages from CRAN}
\description{
  These functions can be used to automatically compare the version
  numbers of installed packages with the newest available version on
  CRAN and update outdated packages on the fly.
}
\usage{
update.packages(lib.loc = NULL, CRAN = getOption("CRAN"),
                contriburl = contrib.url(CRAN),
                method, instlib = NULL,
                ask=TRUE, available=NULL, destdir=NULL,
		installWithVers=FALSE)

installed.packages(lib.loc = NULL, priority = NULL)
CRAN.packages(CRAN = getOption("CRAN"), method,
              contriburl = contrib.url(CRAN))
old.packages(lib.loc = NULL, CRAN = getOption("CRAN"),
             contriburl = contrib.url(CRAN),
             method, available = NULL)

download.packages(pkgs, destdir, available = NULL,
                  CRAN = getOption("CRAN"),
                  contriburl = contrib.url(CRAN), method)
install.packages(pkgs, lib, CRAN = getOption("CRAN"),
                 contriburl = contrib.url(CRAN),
                 method, available = NULL, destdir = NULL,
		 installWithVers = FALSE, dependencies = FALSE)
}
\arguments{
  \item{lib.loc}{character vector describing the location of R
    library trees to search through (and update packages therein).}
  \item{CRAN}{character, the base URL of the CRAN mirror to use, i.e.,
    the URL of a CRAN root such as \code{"http://cran.r-project.org"}
    (the default) or its Statlib mirror,
    \code{"http://lib.stat.cmu.edu/R/CRAN"}.
#ifdef windows
    Can also be \code{NULL} to install from local \code{.zip} files.
#endif
  }
  \item{contriburl}{URL of the contrib section of CRAN. Use this argument
    only if your CRAN mirror is incomplete, e.g., because you burned
    only the contrib section on a CD.  Overrides argument \code{CRAN}.}
  \item{method}{Download method, see \code{\link{download.file}}.}
  \item{pkgs}{character vector of the short names of
    packages whose current versions should be downloaded from \code{CRAN}.}
  \item{destdir}{directory where downloaded packages are stored.}
  \item{priority}{character vector or \code{NULL} (default).  If
    non-null, used to select packages; \code{"high"} is equivalent to
    \code{c("base","recommended")}.  To select all packages without an
    assigned priority, use \code{priority = "NA"}.}
  \item{available}{list of packages available at CRAN as returned by
    \code{CRAN.packages}.}
  \item{lib,instlib}{character string giving the library directory where to
    install the packages.}
  \item{ask}{logical indicating to ask before packages are actually downloaded
    and installed.}
  \item{installWithVers}{If \code{TRUE}, will invoke the install the
    package such that it can be referenced by package version}
  \item{dependencies}{logical indicating to also install uninstalled
    packages on which these packages depend (and so on recursively).
#ifdef windows
    Not used if \code{CRAN = NULL}.
#endif
  }
}
\details{
  \code{installed.packages} scans the \file{DESCRIPTION} files of each
  package found along \code{lib.loc} and returns a list of package
  names, library paths and version numbers.  \code{CRAN.packages}
  returns a similar list, but corresponding to packages currently
  available in the contrib section of CRAN, the comprehensive R archive
  network.  The current list of packages is downloaded over the internet
  (or copied from a local CRAN mirror). Both functions use
  \code{\link{read.dcf}} for parsing the description files.
  \code{old.packages} compares the two lists and reports installed
  packages that have newer versions on CRAN.

  \code{download.packages} takes a list of package names and a
  destination directory, downloads the newest versions of the package
  sources and saves them in \code{destdir}.  If the list of available
  packages is not given as argument, it is also directly obtained from
  CRAN.  If CRAN is local, i.e., the URL starts with \code{"file:"},
  then the packages are not downloaded but used directly.

  The main function of the bundle is \code{update.packages}.  First a
  list of all packages found in \code{lib.loc} is created and compared
  with the packages available on CRAN.  Outdated packages are reported
  and for each outdated package the user can specify if it should be
  automatically updated.
#ifdef unix
  If so, the package sources are downloaded from
  CRAN and installed in the respective library path (or \code{instlib}
  if specified) using the \R \code{\link{INSTALL}} mechanism.
#endif
#ifdef windows
  If so, the pre-compiled packages are
  downloaded from CRAN and installed in the respective library path (or
  \code{instlib} if specified).
#endif

  \code{install.packages} can be used to install new packages, it
  takes a vector of package names and a destination library, downloads
  the packages from CRAN and installs them. If the library
  is omitted it defaults to the first directory in \code{.libPaths()}, with
  a warning if there is more than one.
%  The first argument can be an unquoted name, as in
%  \code{install.packages(nlme)}.
#ifdef windows
  Argument \code{pkgs} can also be a character vector of file names of
  zip files if \code{CRAN=NULL}. The zip files are then unpacked directly.
#endif

  For \code{install.packages} and \code{update.packages}, \code{destdir}
  is the directory to which packages will be downloaded. If it is
  \code{NULL} (the default) a temporary directory is used, and the user
  will be given the option of deleting the temporary files once the
  packages are installed.  (They will always be deleted at the end of
  the \R session.)
}
\seealso{
  See \code{\link{download.file}} for how to handle proxies and
  other options to monitor file transfers.

#ifdef unix
  \code{\link{INSTALL}}, \code{\link{REMOVE}},
#endif
  \code{\link{library}}, \code{\link{.packages}}, \code{\link{read.dcf}}
}
\examples{
str(ip <- installed.packages(priority = "high"))
ip[, c(1,3:5)]
}
\keyword{utilities}
