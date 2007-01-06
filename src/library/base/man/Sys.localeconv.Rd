\name{localeconv}
\alias{localeconv}
\alias{Sys.localeconv}
\title{Find Details of the Numerical and Monetary Representations
  in the Current Locale}
\description{
  Get details of the numerical and monetary representations in the
  current locale.
}
\usage{
Sys.localeconv()
}
\details{
  These settings are usually controlled by the environment variables
  \code{LC_NUMERIC} and \code{LC_MONETARY} and if not set the values of
  \code{LC_ALL} or \code{LANG}.

  Normally \R is run without looking at the value of \code{LC_NUMERIC},
  so the decimal point remains '\code{.}'.  So the first three of these
  values will not be useful unless you have set \code{LC_NUMERIC} in
  the current \R session.
}
\value{
  A character vector with 18 named components.  See your ISO C
  documentation for details of the meaning.

  It is possible to compile \R without support for locales, in which
  case the value will be \code{NULL}.
}
\seealso{
  \code{\link{Sys.setlocale}} for ways to set locales.
}
\examples{
Sys.localeconv()
## The results in the C locale are
##    decimal_point     thousands_sep          grouping   int_curr_symbol 
##              "."                ""                ""                "" 
##  currency_symbol mon_decimal_point mon_thousands_sep      mon_grouping 
##               ""                ""                ""                "" 
##    positive_sign     negative_sign   int_frac_digits       frac_digits 
##               ""                ""             "127"             "127" 
##    p_cs_precedes    p_sep_by_space     n_cs_precedes    n_sep_by_space 
##            "127"             "127"             "127"             "127" 
##      p_sign_posn       n_sign_posn 
##            "127"             "127"

## Now try your default locale (which might be "C").
\dontrun{old <- Sys.getlocale()
Sys.setlocale(locale = "")
Sys.localeconv()
Sys.setlocale(locale = old)}

\dontrun{read.table("foo", dec=Sys.localeconv()["decimal_point"])}
}
\keyword{utilities}
