% File src/library/utils/man/sessionInfo.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{sessionInfo}
\title{Collect Information About the Current R Session}
\alias{sessionInfo}
\alias{osVersion}
\alias{toLatex.sessionInfo}
\alias{print.sessionInfo}
\description{
  Get and report version information about \R, the OS and attached or
  loaded packages.

  The \code{\link{print}()} and \code{\link{toLatex}()} methods (for a
  \code{"sessionInfo"} object) show the locale and timezone information by
  default, when \code{locale} or \code{tzone} are true.
  The \code{system.codepage} is only shown when it is not empty, i.e., only
  on Windows, \emph{and} if it differs from \code{code.page}, see below or
  \code{\link{l10n_info}()}.
}
\usage{
sessionInfo(package = NULL)
\S3method{print}{sessionInfo}(x, locale = TRUE, tzone = locale,
      RNG = !identical(x$RNGkind, .RNGdefaults), \dots)
\S3method{toLatex}{sessionInfo}(object, locale = TRUE, tzone = locale,
        RNG = !identical(object$RNGkind, .RNGdefaults), \dots)
osVersion
}
\arguments{
 \item{package}{a character vector naming installed packages, or \code{NULL}
   (the default) meaning all attached packages.}
  \item{x}{an object of class \code{"sessionInfo"}.}
  \item{object}{an object of class \code{"sessionInfo"}.}
  \item{locale}{show locale, by default \code{tzone}, and (on Windows) code page information?}
  \item{tzone}{show time zone information?}
  \item{RNG}{show information on \code{\link{RNGkind}()}?  Defaults to true
    iff it differs from the \R version's default, i.e., \code{\link{RNGversion}(*)}.}
  \item{\dots}{currently not used.}
}
\value{
  \code{sessionInfo()} returns an object of class \code{"sessionInfo"}
  which has \code{print} and
  \code{\link{toLatex}} methods.  This is a list with components
  \item{R.version}{a list, the result of calling \code{\link{R.Version}()}.}
  \item{platform}{a character string describing the platform \R was
    built under.  Where sub-architectures are in use this is of the form
    \samp{platform/sub-arch}: 32-bit builds have \code{(32-bit)} appended}
  \item{running}{a character string (or possibly \code{NULL}), the same
    as \code{osVersion}, see below.}
  \item{RNGkind}{a character vector, the result of calling \code{\link{RNGkind}()}.}
  \item{matprod}{a character string, the result of calling
    \code{\link{getOption}("matprod")}.}
  \item{BLAS}{a character string, the result of calling
    \code{\link{extSoftVersion}()["BLAS"]}.}
  \item{LAPACK}{a character string, the result of calling \code{\link{La_library}()}.}
  \item{LA_version}{a character string, the result of calling \code{\link{La_version}()}.}
  \item{locale}{a character string, the result of calling \code{\link{Sys.getlocale}()}.}
  \item{tzone}{a character string, the result of calling \code{\link{Sys.timezone}()}.}
  \item{tzcode_type}{a character string indicating source
    (system/internal) of the date-time conversion and printing functions.}
#ifdef windows
  \item{codepage}{an integer, the result of calling
    \code{\link{l10n_info}()["codepage"]}.}
  \item{system.codepage}{an integer, the result of calling
    \code{\link{l10n_info}()["system.codepage"]}.}
#endif
  \item{basePkgs}{a character vector of base packages which are attached.}
  \item{otherPkgs}{(not always present): a named list of the results of
    calling \code{\link{packageDescription}} on other
    attached packages.}
  \item{loadedOnly}{(not always present): a named list of the results of
    calling \code{\link{packageDescription}} on packages whose
    namespaces are loaded but are not attached.}
}
\section{\code{osVersion}}{
  \code{osVersion} is a character string (or possibly \code{NULL} on
  bizarre platforms) describing the OS and version which it is running
  under (as distinct from built under).  This attempts to name a Linux
  distribution and give the OS name on an Apple Mac.

  It is the same as \code{sessionInfo()$running}
  and created when loading the \pkg{utils} package.

  Windows may report unexpected versions: see the help for
  \code{\link{win.version}}.

  How OSes identify themselves and their versions can be arcane: where
  possible \code{osVersion} (and hence \code{sessionInfo()$running}) uses
  a human-readable form.

  Where \R was compiled under macOS 10.x (as the \abbr{CRAN} Intel
  distributions were prior to \R 4.3.0) but running under \sQuote{\I{Big
  Sur}} or later, macOS reports itself as \samp{10.16} (which \R 
  recognizes as \sQuote{\I{Big Sur} ...}) and not \samp{11}, \samp{12}, \ldots{}.
}
\note{
  The information on \sQuote{loaded} packages and namespaces is the
  \emph{current} version installed at the location the package was
  loaded from: it can be wrong if another process has been changing
  packages during the session.
}
\seealso{
  \code{\link{R.version}}, \code{\link{R_compiled_by}}
}
%% FIXME: this should test this, in RDIFF makeup.
\examples{\donttest{
sI <- sessionInfo()
sI
# The same, showing the RNGkind, but not the locale :
  print(sI, RNG = TRUE, locale = FALSE)
toLatex(sI, locale = FALSE) # shortest; possibly desirable at end of report
}}
\keyword{misc}
