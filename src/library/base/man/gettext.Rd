\name{gettext}
\alias{gettext}
\alias{bindtextdomain}
\title{Translate Text Messages}
\description{
  If Native Language Support was enabled in this build of \R, attempt to
  translate character vectors or set where the translations are to be found.
}
\usage{
gettext(\dots, domain = NULL)
bindtextdomain(domain, dirname = NULL)
}
\arguments{
  \item{domain}{The \sQuote{domain} for the translation.}
  \item{\dots}{One or more character vectors.}
  \item{dirname}{The directory in which to find translated message
    catalogs for the domain.}
}
\details{
  If \code{domain} is \code{NULL} or \code{""}, a domain is searched for
  based on the package or namespace which contains the function calling
  \code{gettext}. If a suitable domain can be found, each character
  string is offered for translation, and replaced by its translation
  into the current language if one is found.

  Conventionally the domain for \R warning/error messages in package
  \pkg{pkg} is \code{"R-pkg"}, and that for C-level messages is \code{"pkg"}.

  Leading and trailing whitespace is ignored when looking for the
  translation.
}
\value{
  For \code{gettext}, a character vector, one element per string in
  \code{\dots}.  If translation is not enabled or no domain is found or
  no translation is found in that domain, the original strings are
  returned.

  For \code{bindtextdomain}, a character string giving the current base
  directory, or \code{NULL} if setting it failed.
}
\seealso{
  \code{\link{stop}} and \code{\link{warning}} make use of \code{gettext} to
  translate messages.
}
\examples{
bindtextdomain("R")  # non-null iff NLS is enabled
}
\keyword{ utilities }
\keyword{ character }
