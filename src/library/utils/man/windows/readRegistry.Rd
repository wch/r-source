% File src/library/utils/man/windows/shortPathName.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-10 R Core Team
% Distributed under GPL 2 or later

\name{readRegistry}
\alias{readRegistry}
\title{Read a Windows Registy Hive}
\description{
  Read values of keys in the Windows Registry, and optionally whole hives.
}
\usage{
readRegistry(key, hive = c("HLM", "HCR", "HCU", "HU", "HCC", "HPD"),
             maxdepth = 1, view = c("default", "32-bit", "64-bit"))
}
\arguments{
  \item{key}{character string, the path to the key in the Windows Registry.}
  \item{hive}{The \sQuote{hive} containing the key.  The abbreviations
    are for \code{HKEY_LOCAL_MACHINE},
    \code{HKEY_CLASSES_ROOT}. \code{HKEY_CURRENT_USER},
    \code{HKEY_USERS}, \code{HKEY_CURRENT_CONFIG} and
    \code{HKEY_PERFORMANCE_DATA}}
  \item{maxdepth}{How far to recurse into the subkeys of the key.  By
    default only the values of the key and the names of subkeys are
    returned.}
  \item{view}{On 64-bit Windows, the view of the Registry to be used:
    see \sQuote{Details}.}
}
\details{
  Registry access is done using the security settings of the current \R
  session: this means that some Registry keys may not be accessible even
  if they exist.  This may result in \code{NULL} values in the object
  returned, and, possibly, empty element names.

  On 64-bit Windows, this will by default read the 32-bit view of the
  Registry when run from 32-bit \R, and the 64-bit view when run from
  64-bit \R: see
  \url{http://msdn.microsoft.com/en-us/library/aa384232(VS.85).aspx}.
}
\value{
  A named list of values and subkeys (which may themselves be named
  lists).  The default value (if any) precedes named values which
  precede subkeys, and both the latter sets are sorted alphabetically.
}
\note{
  This is only available on Windows.
}
\examples{\donttest{
## only in HLM if set in an admin-mode install.
readRegistry("SOFTWARE\\\\R-core", maxdepth = 3)

gmt <- file.path("SOFTWARE", "Microsoft", "Windows NT",
                 "CurrentVersion", "Time Zones",
                 "GMT Standard Time", fsep = "\\\\")
readRegistry(gmt, "HLM")

## on a 64-bit R need this to find 32-bit JAGS
readRegistry("SOFTWARE\\\\JAGS-1.0.3", maxdepth = 3, view = "32")

## See if there is a 64-bit user install
readRegistry("SOFTWARE\\\\R-core\\\\R64", "HCU", maxdepth = 2)
}}
\keyword{ utilities }
