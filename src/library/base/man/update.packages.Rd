\name{update.packages}
\alias{update.packages}
\alias{installed.packages}
\alias{CRAN.packages}
\alias{download.file}
\alias{download.packages}
\alias{install.packages}
\title{Download packages from CRAN}}
\usage{
update.packages(lib.loc=.lib.loc, CRAN=options("CRAN"),
                method="auto", instlib=NULL)

installed.packages(lib.loc = .lib.loc)
CRAN.packages(CRAN=options("CRAN"), method="auto")

download.file(url, destfile, method="auto")
download.packages(pkgs, destdir, available=NULL,
                  CRAN=options("CRAN"), method="auto")
install.packages(pkgs, lib, available=NULL,
                  CRAN=options("CRAN"), method="auto")
}
\arguments{ 
 \item{lib.loc}{A character vector describing the location of R
     library trees to search through (and update packages therein).}
 \item{CRAN}{Base URL of the CRAN mirror to use, i.e., URL of a CRAN
     root such as \code{"http://www.r-project.org"} (the default) or
     \code{"http://lib.stat.cmu.edu/R/CRAN"}.}
 \item{method}{Tool to be used for downloading files. Currently download
   methods \code{"auto"}, \code{"wget"}, \code{"lynx"} and \code{"cp"} are
   available.}
 \item{url}{A character string with the URL of a file to be downloaded.}
 \item{destfile}{A character string with the name where the downloaded
     file is saved.}
 \item{pkgs}{A character vector of package names that shall be
     downloaded from CRAN.}
 \item{destdir}{Directory where downloaded packages are stored.}
 \item{available}{List of packages available at CRAN as returned by
     \code{CRAN.packages()}.}
 \item{lib,instlib}{A character string giving the library directory where to
     install the packages.}
}
\description{
    These functions can be used to automatically compare the version
    numbers of installed packages with the newest available version on
    CRAN and update outdated packages on the fly.}
}
\details{
  \code{installed.packages()} scans the DESCRIPTION files of each
  package found along \code{lib.loc} and returns a list of package
  names, library paths and version numbers.  \code{CRAN.packages()}
  returns a similar list, but corresponding to packages currently
  available in the contrib section of CRAN, the comprehensive R archive
  network.  The current list of packages is downloaded over the internet
  (or copied from a local CRAN mirror). Both functions use
  \code{\link{parse.dcf}} for parsing the description files.

  The function \code{download.file()} can be used to download a single
  file as described by \code{url} from the internet and store it in
  \code{destfile}. It makes a system call to the tool given by
  \code{method}, the respective program must be installed on your system
  and be in the search path for executables.  If method \code{"cp"} is
  used, then the \code{url} must start with \code{"file:"} and give the
  path to a local file. Method \code{"auto"} (the default) searches for
  available tools at runtime.
  
  \code{download.packages()} takes a list of package names and a
  destination directory, downloads the newest versions of the package
  sources and saves the in \code{destdir}.  If the list of available
  packages is not given as argument, it is also directly obtained from
  CRAN.  If CRAN is local, i.e., the URL starts with \code{"file:"},
  then the packages are not downloaded but used directly.
    
  The main function of the bundle is \code{update.packages()}.  First a
  list of all packages found in \code{lib.loc} is created and compared
  with the packages available on CRAN.  Outdated packages are reported
  and for each outdated package the user can specify if it should be
  automatically updated.  If so, the package sources are downloaded from
  CRAN and installed in the respective library path (or \code{instlib}
  if specified) using the \R \code{\link{INSTALL}} mechanism.

  \code{install.packages()} can be used to install new packages, it
  takes a vector of package names and a destination directory, downloads
  the packages from CRAN and installs them. If the destination directory
  is omitted it defaults to the first directory in \code{.lib.loc}, with
  a warning.
}
\seealso{\code{\link{INSTALL}}, \code{\link{REMOVE}},
  \code{\link{library}}, \code{\link{.packages}},
  \code{\link{parse.dcf}}
}
\keyword{utilities}
