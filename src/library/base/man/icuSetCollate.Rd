% File src/library/base/man/icuSetCollate.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-9 R Core Team
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
    \item{\code{locale}:}{A character string such as \code{"da_DK"} giving the
      country whose collation rules are to be used.  If present, this
      should be the first argument.}
    \item{\code{case_first}:}{\code{"upper"}, \code{"lower"} or
      \code{"default"}, asking for upper- or lower-case characters to be
      sorted first.  The default is usually lower-case first, but not in
      all languages (see the Danish example).}
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

  Some examples are \code{case_level = "on", strength = "primary"} to ignore
  accent differences and \code{alternate_handling = "shifted"} to ignore
  space and punctuation characters.

  Note that these settings have no effect if collation is set to the
  \code{C} locale, unless \code{locale} is specified.
}
\note{
  ICU is used by default wherever it is available:
  this include Mac OS >= 10.4 and many Linux installations.
}
\seealso{
  \link{Comparison}, \code{\link{sort}}

  The ICU user guide chapter on collation
  (\url{http://userguide.icu-project.org/collation}).
}
\examples{\donttest{
## these examples depend on having ICU available, and on the locale
x <- c("Aarhus", "aarhus", "safe", "test", "Zoo")
sort(x)
icuSetCollate(case_first = "upper"); sort(x)
icuSetCollate(case_first = "lower"); sort(x)

icuSetCollate(locale = "da_DK", case_first = "default"); sort(x)
icuSetCollate(locale = "et_EE"); sort(x)
}}
\keyword{ utilities }
