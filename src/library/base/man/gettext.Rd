\name{gettext}
\alias{gettext}
\alias{ngettext}
\alias{bindtextdomain}
\title{Translate Text Messages}
\description{
  If Native Language Support was enabled in this build of \R, attempt to
  translate character vectors or set where the translations are to be found.
}
\usage{
gettext(\dots, domain = NULL)

ngettext(n, msg1, msg2, domain = NULL)

bindtextdomain(domain, dirname = NULL)
}
\arguments{
  \item{\dots}{One or more character vectors.}
  \item{domain}{The \sQuote{domain} for the translation.}
  \item{n}{a non-negative integer.}
  \item{msg1}{the message to be used in English for \code{n = 1}.}
  \item{msg2}{the message to be used in English for \code{n = 0, 2, 3,\dots}.}
  \item{dirname}{The directory in which to find translated message
    catalogs for the domain.}
}
\details{
  If \code{domain} is \code{NULL} or \code{""}, a domain is searched for
  based on the namespace which contains the function calling
  \code{gettext} or \code{ngettext}.  If a suitable domain can be found,
  each character string is offered for translation, and replaced by its
  translation into the current language if one is found.

  Conventionally the domain for \R warning/error messages in package
  \pkg{pkg} is \code{"R-pkg"}, and that for C-level messages is \code{"pkg"}.

  For \code{gettext}, leading and trailing whitespace is ignored when
  looking for the translation.

  \code{ngettext} is used where the message needs to vary by a single
  integer.  Translating such messages is subject to very specific rules
  for different languages: see the GNU Gettext Manual.  The string
  will often contain a single instance of \code{\%d} to be used in
  \code{\link{sprintf}}.  If English is used, \code{msg1} is returned if
  \code{n == 1} and \code{msg2} in all other cases.
}
\value{
  For \code{gettext}, a character vector, one element per string in
  \code{\dots}.  If translation is not enabled or no domain is found or
  no translation is found in that domain, the original strings are
  returned.

  For \code{ngettext}, a character string.

  For \code{bindtextdomain}, a character string giving the current base
  directory, or \code{NULL} if setting it failed.
}
\seealso{
  \code{\link{stop}} and \code{\link{warning}} make use of \code{gettext} to
  translate messages.

  \code{\link{xgettext}} for extracting translatable strings from \R
  source files. 
}
\examples{
bindtextdomain("R")  # non-null iff NLS is enabled

for(n in 0:3)
    print(sprintf(ngettext(n, "\%d variable has missing values",
                              "\%d variables have missing values"),
                  n))

\dontrun{## for translation, those strings should appear in R-pkg.pot as
msgid        "\%d variable has missing values"
msgid_plural "\%d variables have missing values"
msgstr[0] ""
msgstr[1] ""
}

miss <- c("one", "or", "another")
cat(ngettext(length(miss), "variable", "variables"),
    paste(sQuote(miss), collapse=", "),
    ngettext(length(miss), "contains", "contain"), "missing values\n")

## better for translators would be to use
cat(sprintf(ngettext(length(miss),
                     "variable \%s contains missing values\n",
                     "variables \%s contain missing values\n"),
            paste(sQuote(miss), collapse=", ")))
}
\keyword{ utilities }
\keyword{ character }
