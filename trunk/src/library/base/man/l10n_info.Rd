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
}
\seealso{
\code{\link{Sys.getlocale}}, \code{\link{localeconv}}
}
\examples{
l10n_info()
}
\keyword{ utilities }
