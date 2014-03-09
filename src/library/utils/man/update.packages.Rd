% File src/library/utils/man/update.packages.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{update.packages}
\alias{update.packages}
\alias{old.packages}
\alias{new.packages}
\title{Compare Installed Packages with CRAN-like Repositories}
\description{
  \code{old.packages} indicates packages which have a (suitable) later
  version on the repositories whereas \code{update.packages} offers to
  download and install such packages.

  \code{new.packages} looks for (suitable) packages on the repositories
  that are not already installed, and optionally offers them for
  installation.
}
\usage{
update.packages(lib.loc = NULL, repos = getOption("repos"),
                contriburl = contrib.url(repos, type),
                method, instlib = NULL,
                ask = TRUE, available = NULL,
                oldPkgs = NULL, \dots, checkBuilt = FALSE,
                type = getOption("pkgType"))

old.packages(lib.loc = NULL, repos = getOption("repos"),
             contriburl = contrib.url(repos, type),
             instPkgs = installed.packages(lib.loc = lib.loc),
             method, available = NULL, checkBuilt = FALSE,
             type = getOption("pkgType"))

new.packages(lib.loc = NULL, repos = getOption("repos"),
             contriburl = contrib.url(repos, type),
             instPkgs = installed.packages(lib.loc = lib.loc),
             method, available = NULL, ask = FALSE, \dots,
             type = getOption("pkgType"))
}
\arguments{
  \item{lib.loc}{character vector describing the location of R
    library trees to search through (and update packages therein), or
    \code{NULL} for all known trees (see \code{\link{.libPaths}}).}
  \item{repos}{
    character vector, the base URL(s) of the repositories
    to use, e.g., the URL of a CRAN mirror such as
    \code{"http://cran.us.r-project.org"}.
  }
  \item{contriburl}{URL(s) of the contrib sections of the
    repositories.  Use this argument if your repository is
    incomplete.  Overrides argument \code{repos}.
  }
  \item{method}{Download method, see \code{\link{download.file}}.}
  \item{instlib}{character string giving the library directory where to
    install the packages.}
  \item{ask}{logical indicating whether to ask user before packages are
    actually downloaded and installed, or the character string
    \code{"graphics"}, which brings up a widget to allow the
    user to (de-)select from the list of packages which could
    be updated or added.  The latter value only works on
    systems with a GUI version of \code{\link{select.list}},
    and is otherwise equivalent to \code{ask = TRUE}.
  }
  \item{available}{
    an object as returned by \code{\link{available.packages}}
    listing packages available at the repositories, or \code{NULL} which
    makes an internal call to \code{available.packages}.
  }
  \item{checkBuilt}{If \code{TRUE}, a package built under an earlier
    major.minor version of \R (e.g. \code{3.1}) is considered to be
    \sQuote{old}.}
  \item{oldPkgs}{
    if specified as non-NULL, \code{update.packages()} only considers
    these packages for updating. This may be a character vector
    of package names or a matrix as returned by \code{old.packages()}.
  }
  \item{instPkgs}{
    by default all installed packages,
    \code{\link{installed.packages}(lib.loc = lib.loc)}.  A subset can be
    specified; currently this must be in the same (character matrix)
    format as returned by \code{installed.packages()}.
  }
  \item{\dots}{
    Arguments such as \code{destdir} and \code{dependencies} to be
    passed to \code{\link{install.packages}}.
  }
  \item{type}{character, indicating the type of package to download and
    install.  See \code{\link{install.packages}}.
  }
}
\details{
  \code{old.packages} compares the information from
  \code{\link{available.packages}} with that from \code{instPkgs} (computed by
  \code{\link{installed.packages}} by default) and reports installed
  packages that have newer versions on the repositories or, if
  \code{checkBuilt = TRUE}, that were built under an earlier minor
  version of \R (for example built under 3.0.x when running \R 3.1.0).
  (For binary package types here is no check that the version on the
  repository was built under the current minor version of \R,
  but it is advertised as being suitable for this version.)

  \code{new.packages} does the same comparison but reports uninstalled
  packages that are available at the repositories.  If \code{ask !=
  FALSE} it asks which packages should be installed in the first element
  of \code{lib.loc}.

  The main function of the set is \code{update.packages}.  First a list
  of all packages found in \code{lib.loc} is created and compared with
  those available at the repositories.  If \code{ask = TRUE} (the
  default) packages with a newer version are reported and for each one
  the user can specify if it should be updated.  If so the packages are
  downloaded from the repositories and installed in the respective
  library path (or \code{instlib} if specified).

  For how the list of suitable available packages is determined see
  \code{\link{available.packages}}.  \code{available = NULL} make a call
  to \code{available.packages(contriburl = contriburl, method = method)}
  and hence by default filters on \R version, OS type and removes
  duplicates.
}
\value{
  \code{update.packages} returns \code{NULL} invisibly.

  For \code{old.packages}, \code{NULL} or a matrix with one row per
  package, row names the package names and column names
  \code{"Package"}, \code{"LibPath"}, \code{"Installed"} (the version),
  \code{"Built"} (the version built under), \code{"ReposVer"} and
  \code{"Repository"}.

  For \code{new.packages} a character vector of package names,
  \emph{after} any selected \emph{via} \code{ask} have been installed.
}
\section{Warning}{
  Take care when using \code{dependencies} (passed to
  \code{\link{install.packages}}) with \code{update.packages},
  for it is unclear where new dependencies should be installed.  The
  current implementation will only allow it if all the packages to be
  updated are in a single library, when that library will be used.
}
\seealso{
  \code{\link{install.packages}},
  \code{\link{available.packages}}, \code{\link{download.packages}},
  \code{\link{installed.packages}}, \code{\link{contrib.url}}.

  See \code{\link{download.file}} for how to handle proxies and
  other options to monitor file transfers.

  \code{\link{INSTALL}}, \code{\link{REMOVE}}, \code{\link{remove.packages}},
  \code{\link{library}}, \code{\link{.packages}}, \code{\link{read.dcf}}

  The \sQuote{R Installation and Administration} manual for how to
  set up a repository.
}

\keyword{utilities}
