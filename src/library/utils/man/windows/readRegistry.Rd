\name{readRegistry}
\alias{readRegistry}
\title{Read a Windows Registy Hive}
\description{
  Read values of keys in the Windows Registry, and optionally whole hives.
}
\usage{
readRegistry(key, hive = c("HLM", "HCR", "HCU", "HU", "HCC, HPD"),
             maxdepth = 1)
}
\arguments{
  \item{key}{character string, the path to the key in the Windows Registry.}
  \item{hive}{The \sQuote{hive} containing the key.  The abbreviations
    are for \code{HKEY_LOCAL_MACHINE},
    \code{HKEY_CLASSES_ROOT}. \code{HKEY_CURRENT_USER},
    \code{HKEY_USERS}. \code{HKEY_CURRENT_CONFIG} and
    \code{HKEY_PERFORMANCE_DATA}}
  \item{maxdepth}{How far to recurse into the subkeys of the key.  By
    default only the values of the key and the names of subkeys are
    returned.}
}
\details{
  Registry access is done using the security settings of the current \R
  session: this means that some Registry keys may not be accessible even
  if they exist.  This may result in \code{NULL} values in the object
  return, and, possibly, empty element names.
}
\value{
  A named list of values and subkeys (which may themselves be named
  lists).  The default value (if any) precedes named values which
  precede subkeys, and both the latter sets are sorted alphabetically.
}
\examples{\donttest{
readRegistry("SOFTWARE\\\\R-core", maxdepth = 3)
gmt <- file.path("SOFTWARE", "Microsoft", "Windows NT",
                 "CurrentVersion", "Time Zones",
                 "GMT Standard Time", fsep="\\\\")
readRegistry(gmt, "HLM")
}}
\keyword{ utilities }
