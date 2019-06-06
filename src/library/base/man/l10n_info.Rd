% File src/library/base/man/l10n_info.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{l10n_info}
\alias{l10n_info}
\title{Localization Information}
\description{
  Report on localization information.
}
\usage{
l10n_info()
}
\value{
  A list with three logical components:
  \item{MBCS}{If a multi-byte character set in use?}
  \item{UTF-8}{Is this a UTF-8 locale?}
  \item{Latin-1}{Is this a Latin-1 locale?}
#ifdef windows
  \item{codepage}{integer: the Windows codepage corresponding to the
    locale \R is using (and not necessarily that Windows is using).}
  \item{system.codepage}{integer: the Windows system/ANSI codepage
    (the codepage Windows is using).}
#endif
}
#ifdef windows
\details{
  Common codepages are 1252 (Western European), 1250 (Central European),
  1251 (Cyrillic), 1253 (Greek), 1254 (Turkish), 1255 (Hebrew), 1256
  (Arabic), 1257 (Baltic), 1258 (Vietnamese), 874 (Thai), 932
  (Japanese), 936 (Simplified Chinese), 949 (Korean) and 950
  (Traditional Chinese).  \R does not allow the C locale, and uses 1252
  as the default codepage.
}
#endif
\seealso{
\code{\link{Sys.getlocale}}, \code{\link{localeconv}}
}
\examples{
l10n_info()
}
\keyword{ utilities }
