% File src/library/base/man/icuSetCollate.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-2014 R Core Team
% Distributed under GPL 2 or later

\name{icuSetCollate}
\alias{icuSetCollate}
\title{ Setup Collation by ICU }
\description{
  Controls the way collation is done by ICU (an optional part of the \R
  build).
}
\usage{
icuSetCollate(...)
}
\arguments{
  \item{\dots}{Named arguments, see \sQuote{Details}.}
}
\details{
  Optionally, \R can be built to collate character strings by ICU
  (\url{http://site.icu-project.org}).  For such systems,
  \code{icuSetCollate} can be used to tune the way collation is done.
  On other builds calling this function does nothing, with a warning.

  Possible arguments are
  \describe{
    \item{\code{locale}:}{A character string such as \code{"da_DK"}
      giving the language and country whose collation rules are to be
      used.  If present, this should be the first argument.}
    \item{\code{case_first}:}{\code{"upper"}, \code{"lower"} or
      \code{"default"}, asking for upper- or lower-case characters to be
      sorted first.  The default is usually lower-case first, but not in
      all languages.}
    \item{\code{alternate_handling}:}{Controls the handling of
      \sQuote{variable} characters (mainly punctuation and symbols).
      Possible values are \code{"non_ignorable"} (primary strength) and
      \code{"shifted"} (quaternary strength).}
    \item{\code{strength}:}{Which components should be used?  Possible
      values \code{"primary"}, \code{"secondary"}, \code{"tertiary"}
      (default), \code{"quaternary"} and \code{"identical"}. }
    \item{\code{french_collation}:}{In a French locale the way accents
      affect collation is from right to left, whereas in most other locales
      it is from left to right.  Possible values \code{"on"}, \code{"off"}
      and \code{"default"}.}
    \item{\code{normalization}:}{Should strings be normalized?  Possible values
      are \code{"on"} and \code{"off"} (default).  This affects the
      collation of composite characters.}
    \item{\code{case_level}:}{An additional level between secondary and
      tertiary, used to distinguish large and small Japanese Kana
      characters. Possible values \code{"on"} and \code{"off"} (default).}
    \item{\code{hiragana_quaternary}:}{Possible values \code{"on"} (sort
      Hiragana first at quaternary level) and \code{"off"}.}
  }
  Only the first three are likely to be of interest except to those with a
  detailed understanding of collation and specialized requirements.

  Some special values are accepted for \code{locale}:
  \describe{
    \item{\code{"none"}:}{ICU is not used for collation: the OS's
      collation services are used instead.  (As from \R 3.1.2.)}
    \item{\code{"default"}:}{
      obtains the locale from the OS as is done at the start of the
      session. (As from \R 3.1.2:
      \code{icuSetCollation(locale = Sys.getlocale("LC_COLLATE"))}
      can be used in earlier versions.)
    }
    \item{\code{""}, \code{"root"}:}{
      the \sQuote{root} collation: see
      \url{http://www.unicode.org/reports/tr35/tr35-collation.html#Root_Collation}.
    }
  }
  For the specifications of \sQuote{real} ICU locales, see
  \url{http://userguide.icu-project.org/locale}.  Note that ICU does not
  report that a locale is not supported, but fallbacks to its idea of
  \sQuote{best fit} (which could be rather different).
  
  Some examples are \code{case_level = "on", strength = "primary"} to ignore
  accent differences and \code{alternate_handling = "shifted"} to ignore
  space and punctuation characters.
  
  Initially ICU will not be used for collation if the OS is set to use
  the \code{C} locale for collation.  Once this function is called with
  a value for \code{locale}, ICU will be used until it is called again
  with \code{locale = "none"}.

  All customizations are reset to the default for the locale if
  \code{locale} is specified: the collation engine is reset if the
  OS collation locate category is changed by \code{\link{Sys.setlocale}}.
}
\note{
  ICU is used by default wherever it is available: this include OS X,
  Solaris and many Linux installations.  As it works internally in
  UTF-8, it will be most efficient in UTF-8 locales.

  It is optional on Windows: if \R has been built against ICU, it will
  only be used once \code{icuSetCollate} is called to select the locale
  (as ICU and Windows differ in their idea of locale names).
}
\seealso{
  \link{Comparison}, \code{\link{sort}}.

  \code{\link{capabilities}} for whether ICU is in use.

  The ICU user guide chapter on collation
  (\url{http://userguide.icu-project.org/collation}).
}
\examples{\donttest{
## These examples depend on having ICU available, and on the locale.
## As we don't know the current settings, we can only reset to the default.
if(capabilities("ICU")) {
    x <- c("Aarhus", "aarhus", "safe", "test", "Zoo")
    print(sort(x))
    icuSetCollate(case_first = "upper"); print(sort(x))
    icuSetCollate(case_first = "lower"); print(sort(x))

    icuSetCollate(locale = "da_DK", case_first = "default"); print(sort(x))
    icuSetCollate(locale = "et_EE"); print(sort(x))
    icuSetCollate(locale = "default")
}
}}
\keyword{ utilities }
