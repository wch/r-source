% File src/library/base/man/gettext.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{gettext}
\title{Translate Text Messages}
\alias{gettext}
\alias{ngettext}
\alias{bindtextdomain}
\alias{Sys.setLanguage}
\description{
  Translation of text messages typically from calls to
  \code{\link{stop}()}, \code{\link{warning}()}, or \code{\link{message}()}
  happens when Native Language Support (\abbr{NLS}) was enabled in this build of
  \R as it is almost always, see also the \code{bindtextdomain()} example.

  The functions documented here are the low level building blocks used
  explicitly or implicitly in almost all such message producing calls and
  they attempt to
  translate character vectors or set where the translations are to be found.
}
\usage{
gettext(\dots, domain = NULL, trim = TRUE)

ngettext(n, msg1, msg2, domain = NULL)

bindtextdomain(domain, dirname = NULL)

Sys.setLanguage(lang, unset = "en") %, force = FALSE
}
\arguments{
  \item{\dots}{one or more character vectors.}
  \item{trim}{logical indicating if the white space trimming in
    \code{gettext()} should happen.  \code{trim = FALSE} may be needed for
    compiled code (C / C++) messages which often end with \code{\\n}.}
  \item{domain}{the \sQuote{domain} for the translation, a \code{character}
    string, or \code{\link{NULL}}; see \sQuote{Details}.}
  \item{n}{a non-negative integer.}
  \item{msg1}{the message to be used in English for \code{n = 1}.}
  \item{msg2}{the message to be used in English for \code{n = 0, 2, 3, \dots}.}
  \item{dirname}{the directory in which to find translated message
    catalogs for the domain.}
  \item{lang}{a \code{\link{character}} string specifying a language for
    which translations should be sought.}
  \item{unset}{a string, specifying the default language assumed to be
    current in the case \code{\link{Sys.getenv}("LANGUAGE")} is unset or
    empty.}
% \item{force}{logical indicating if in case of a \dQuote{hard} set
%   \code{"C"} locale, \code{Sys.setLanguage()} should try modifying the
%   locale (to \code{"en"}) in order to subsequently set the desired language
%   \code{lang}, currently only when \code{\link{.Platform}[["OS.type"]]}
%   is \code{"unix"}.}
}
\details{
  If \code{domain} is \code{NULL} (the default) in \code{gettext}
  or \code{ngettext}, the domain is inferred.  If \code{gettext}
  or \code{ngettext}  is called from a function in the namespace of
  package \pkg{pkg} including called via \code{\link{stop}()},
  \code{\link{warning}()}, or \code{\link{message}()} from the function,
  or, say, evaluated as if called from that namespace, see the
  \code{evalq()} example, % and more in ../../../../tests/reg-translation.R
  the domain is set to \code{"R-pkg"}.  Otherwise there is no default
  domain and messages are not translated.

  Setting \code{domain = NA} in \code{gettext} or \code{ngettext}
  suppresses any translation.

  \code{""} does not match any domain.  In \code{gettext} or \code{ngettext},
  \code{domain = ""} is effectively the same as \code{domain = NA}.

  If the domain is found, each character string is offered for
  translation, and replaced by its translation into the current language
  if one is found.

  The \emph{language} to be used for message translation is determined by
  your OS default and/or the locale setting at \R's startup, see
  \code{\link{Sys.getlocale}()}, and notably the \env{LANGUAGE} environment
  variable, and also \code{Sys.setLanguage()} here.

  Conventionally the domain for \R warning/error messages in package
  \pkg{pkg} is \code{"R-pkg"}, and that for C-level messages is \code{"pkg"}.

  For \code{gettext}, when \code{trim} is true as by default,
  leading and trailing whitespace is ignored (\dQuote{trimmed}) when
  looking for the translation.

  \code{ngettext} is used where the message needs to vary by a single
  integer.  Translating such messages is subject to very specific rules
  for different languages: see the GNU \verb{gettext} manual.  The string
  will often contain a single instance of \code{\%d} to be used in
  \code{\link{sprintf}}.  If English is used, \code{msg1} is returned if
  \code{n == 1} and \code{msg2} in all other cases.

  \code{bindtextdomain} is typically a wrapper for the C function of the same
  name: your system may have a \command{man} page for it.  With a
  non-\code{NULL} \code{dirname} it specifies where to look for message
  catalogues: with \code{dirname = NULL} it returns the current location.
  If \abbr{NLS} is not enabled, \code{bindtextdomain(*,*)} returns \code{NULL}.
  %%
  The special case \code{bindtextdomain(NULL)} calls C level
  \code{textdomain(textdomain(NULL))} for the purpose of flushing (i.e.,
  emptying) the cache of already translated strings; it returns \code{TRUE}
  when \abbr{NLS} is enabled.

  The utility \code{Sys.setLanguage(lang)} combines setting the
  \env{LANGUAGE} environment variable with flushing the translation cache
  by \code{bindtextdomain(NULL)}.
}
\value{
  For \code{gettext}, a character vector, one element per string in
  \code{\dots}.  If translation is not enabled or no domain is found or
  no translation is found in that domain, the original strings are
  returned.

  For \code{ngettext}, a character string.

  For \code{bindtextdomain}, a character string giving the current base
  directory, or \code{NULL} if setting it failed.

  For \code{Sys.setLanguage()}, the previous \env{LANGUAGE} setting with
  attribute \code{\link{attr}(*, "ok")}, a \code{\link{logical}}
  indicating success.
  Note that currently, using a non-existing language \code{lang} is still
  set and no translation will happen, without any \code{\link{message}}.
}
\section{Warning}{
  These functions were written assuming that environment variable
  \env{LANGUAGE} is supported by the implementation of \code{libintl}.
  This is almost universally true and (along with \command{gettext}
  \abbr{et al.}) became part of the 2024 POSIX specification.
  In 2025, however, it was still ignored by the
  implementation in \code{musl} (one of the implementations available on
  Alpine Linux).
}

\seealso{
  \code{\link{stop}} and \code{\link{warning}} make use of \code{gettext} to
  translate messages.

  \code{\link{xgettext}} (package \pkg{tools}) for extracting translatable
  strings from \R source files.
}
\examples{
bindtextdomain("R")  # non-null if and only if NLS is enabled

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

miss <- "One only" # this line, or the next for the ngettext() below
miss <- c("one", "or", "another")
cat(ngettext(length(miss), "variable", "variables"),
    paste(sQuote(miss), collapse = ", "),
    ngettext(length(miss), "contains", "contain"), "missing values\n")

## better for translators would be to use
cat(sprintf(ngettext(length(miss),
                     "variable \%s contains missing values\n",
                     "variables \%s contain missing values\n"),
            paste(sQuote(miss), collapse = ", ")))

thisLang <- Sys.getenv("LANGUAGE", unset = NA) # so we can reset it
if(is.na(thisLang) || !nzchar(thisLang)) thisLang <- "en" # "factory" default
enT <- "empty model supplied"
Sys.setenv(LANGUAGE = "de") # may not always 'work'
gettext(enT, domain="R-stats")# "leeres Modell angegeben" (if translation works)
tget <- function() gettext(enT)
tget() # not translated as fn tget() is not from "stats" pkg/namespace
evalq(function() gettext(enT), asNamespace("stats"))() # *is* translated

## Sys.setLanguage()  -- typical usage --
Sys.setLanguage("en") -> oldSet # does set LANGUAGE env.var
errMsg <- function(expr) tryCatch(expr, error=conditionMessage)
(errMsg(1 + "2") -> err)
Sys.setLanguage("fr")
errMsg(1 + "2")
Sys.setLanguage("de")
errMsg(1 + "2")
## Usually, you would reset the language to "previous" via
Sys.setLanguage(oldSet)

\dontdiff{## A show off of translations -- platform (font etc) dependent:
## The translation languages available for "base" R in this version of R:
if(capabilities("NLS")) withAutoprint({
  langs <- list.files(bindtextdomain("R"),
		      pattern = "^[a-z]{2}(_[A-Z]{2}|@quot)?$")
  langs
  txts <- sapply(setNames(,langs),
		 function(lang) { Sys.setLanguage(lang)
				 gettext("incompatible dimensions", domain="R-stats") })
  cbind(txts)
  (nTrans <- length(unique(txts)))
  (not_translated <- names(txts[txts == txts[["en"]]]))
})
}
## Here, we reset to the *original* setting before the full example started:
if(nzchar(thisLang)) { ## reset to previous and check
  Sys.setLanguage(thisLang)
  stopifnot(identical(errMsg(1 + "2"), err))
} # else staying at 'de' ..
}
\keyword{ utilities }
\keyword{ character }
