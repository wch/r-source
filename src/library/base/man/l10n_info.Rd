% File src/library/base/man/l10n_info.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
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
  A list with three logical elements and further OS-specific elements:
  \item{MBCS}{If a multi-byte character set in use?}
  \item{UTF-8}{Is this known to be a UTF-8 locale?}
  \item{Latin-1}{Is this known to be a Latin-1 locale?}

  \emph{Not} on Windows:

  \item{codeset}{character.  The encoding name as reported by the OS,
    possibly \code{""}. (Added in \R 4.1.0.  Encoding names are
    OS-specific.)}

  Only on Windows:

  \item{codepage}{integer: the Windows codepage corresponding to the
    locale \R is using (and not necessarily that Windows is using).}
  \item{system.codepage}{integer: the Windows system/ANSI codepage
    (the codepage Windows is using).  Added in \R 4.1.0.}
}
\details{
  %% On Windows this looks at the codepages as returned by setlocale(LC_CTYPE)
  %%  and getACP (for system.codepage).
  %% Elsewhere it uses nl_langinfo(CODESET).
  \sQuote{A Latin-1 locale} includes supersets (for printable
  characters) such as Windows codepage 1252 but not Latin-9 (ISO 8859-15).

  On \bold{Windows} (where the resulting list contains \code{codepage}
  and \code{system.codepage} components additionally), common
  codepages are 1252 (Western European), 1250 (Central European),
  1251 (Cyrillic), 1253 (Greek), 1254 (Turkish), 1255 (Hebrew), 1256
  (Arabic), 1257 (Baltic), 1258 (Vietnamese), 874 (Thai), 932
  (Japanese), 936 (Simplified Chinese), 949 (Korean) and 950
  (Traditional Chinese).  Codepage 28605 is Latin-9 and 65001 is UTF-8
  (where supported).  \R does not allow the C locale, and uses 1252 as
  the default codepage.
}
\seealso{
\code{\link{Sys.getlocale}}, \code{\link{localeconv}}
}
\examples{
l10n_info()
}
\keyword{ utilities }
