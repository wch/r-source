\name{update.packages}
\alias{update.packages}
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
                ask = TRUE, available = NULL, destdir = NULL,
		installWithVers = FALSE)

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
    packages whose current versions should be downloaded from
    \code{CRAN}.
#ifdef windows
    If \code{CRAN = NULL}, a vector of file names of \code{.zip} files.
#endif
  }
  \item{destdir}{directory where downloaded packages are stored.}
  \item{available}{list of packages available at CRAN as returned by
    \code{CRAN.packages}.}
  \item{lib}{character vector giving the library directories where to
    install the packages.  Recycled as needed.}
  \item{ask}{logical indicating to ask before packages are actually
    downloaded and installed.}
  \item{installWithVers}{If \code{TRUE}, will invoke the install the
    package such that it can be referenced by package version}
  \item{instlib}{character string giving the library directory where to
    install the packages.}
  \item{dependencies}{logical indicating to also install uninstalled
    packages on which these packages depend/suggest/import
    (and so on recursively).
#ifdef windows
    Not used if \code{CRAN = NULL}.
#endif
  }
}
\details{
  \code{CRAN.packages} returns a matrix of package details corresponding
  to packages/bundles currently available in the contrib section of
  CRAN, the comprehensive R archive network.  The current list of
  packages is downloaded over the internet (or copied from a local CRAN
  mirror).
  
  \code{old.packages} compares the information from \code{CRAN.packages}
  with that from \code{\link{installed.packages}} and reports installed
  packages/bundles that have newer versions on CRAN.

  \code{download.packages} takes a list of package/bundle names and a
  destination directory, downloads the newest versions of the package
  sources and saves them in \code{destdir}.  If the list of available
  packages is not given as argument, it is also directly obtained from
  CRAN.  If CRAN is local, i.e., the URL starts with \code{"file:"},
  then the packages are not downloaded but used directly.

  The main function of the bundle is \code{update.packages}.  First a
  list of all packages found in \code{lib.loc} is created and compared
  with the packages available on CRAN.  Packages with a newer version
  are reported and for each one the user can specify if it should be
  updated.
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

  \code{install.packages} can be used to install new packages.  It
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
#ifdef unix
  An attempt is made to install the packages in an order that respects
  their dependencies.  This does assume that all the entries in
  \code{lib} are on the default library path for installs (set by
  \code{R_LIBS}). 
#endif

  For \code{install.packages} and \code{update.packages}, \code{destdir}
  is the directory to which packages will be downloaded. If it is
  \code{NULL} (the default) a temporary directory is used, and the user
  will be given the option of deleting the temporary files once the
  packages are installed.  (They will always be deleted at the end of
  the \R session.)
}
\value{
  For \code{CRAN.packages}, a matrix with one row per package/bundle,
  row names the package names and column names \code{"Package"}, 
  \code{"Version"}, \code{"Priority"}, \code{"Bundle"},
  \code{"Depends"}, \code{"Imports"}, \code{"Suggests"}
  and \code{"Contains"}.

  For \code{old.packages}, a matrix with one row per package/bundle
  and column names \code{"Package"}, \code{"LibPath"},
  \code{"Installed"} and \code{"CRAN"}.  (The last two columns are
  version numbers.)

  For \code{download.packages}, a two-column matrix of names and
  destination file names, for those packages/bundles successfully downloaded.

  \code{install.packages} and \code{update.packages} have no return value.
}
\seealso{
  \code{\link{installed.packages}}.
  
  See \code{\link{download.file}} for how to handle proxies and
  other options to monitor file transfers.

  \code{\link{INSTALL}}, \code{\link{REMOVE}},
  \code{\link{library}}, \code{\link{.packages}}, \code{\link{read.dcf}}
}
\keyword{utilities}
