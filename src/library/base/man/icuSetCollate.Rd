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
  (\url{http://www.icu-project.org}.  For such systems,
  \code{icuSetCollate} can be used to tune the way collation is done.
  On other build, calling this function does nothing, with a warning.
  
  Possible arguments are
  \itemize{
    \item{\code{locale}:}{A character string such as \code{"da_DK"} giving the
      country whose collation rules are to be used.  If present, this
      should be the first argument.}
    \item{\code{case_first}:}{\code{"upper"}, \code{"lower"} or
      \code{"default"}, asking for upper- or lower-case characters to be
      sorted first.  The default is usually lower-case first, but not in
      all languages.}
    \item{\code{french_collation}:}{In a French locale the way accents
      affect collation is from right to left, whereas in most other locales
      it is from left to right.  Possible values \code{"on"}, \code{"off"}
      and \code{"default"}.}
  }
  Others may be added in future.
}
\seealso{
  \link{Comparison}, \code{\link{sort}}
}
\examples{\donttest{
x <- c("Aarhus", "aarhus", "safe", "test", "Zoo")
sort(x)
icuSetCollate(case_first="upper"); sort(x)
icuSetCollate(case_first="lower"); sort(x)

icuSetCollate(locale="da_DK", case_first="default"); sort(x)
icuSetCollate(locale="et_EE"); sort(x)
}}
\keyword{ utilities }
