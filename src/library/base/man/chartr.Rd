\name{chartr}
\alias{chartr}
\alias{tolower}
\alias{toupper}
\alias{casefold}
\title{Character Translation and Casefolding}
\description{
  Translate characters in character vectors, in particular from upper to
  lower case or vice versa.
}
\usage{
chartr(old, new, x)
tolower(x)
toupper(x)
casefold(x, upper = FALSE)
}
\arguments{
  \item{x}{a character vector.}
  \item{old}{a character string specifying the characters to be
    translated.}
  \item{new}{a character string specifying the translations.}
  \item{upper}{logical: translate to upper or lower case?.}
}
\details{
  \code{chartr} translates each character in \code{x} that is specified
  in \code{old} to the corresponding character specified in \code{new}.
  Ranges are supported in the specifications, but character classes and
  repeated characters are not.  If \code{old} contains more characters
  than new, an error is signaled; if it contains fewer characters, the
  extra characters at the end of \code{new} are ignored.

  \code{tolower} and \code{toupper} convert upper-case characters in a
  character vector to lower-case, or vice versa.  Non-alphabetic
  characters are left unchanged.

  \code{casefold} is a wrapper for \code{tolower} and \code{toupper}
  provided for compatibility with S-PLUS.
}
\seealso{\code{\link{sub}} and \code{\link{gsub}} for other
  substitutions in strings.}
\examples{
x <- "MiXeD cAsE 123"
chartr("iXs", "why", x)
chartr("a-cX", "D-Fw", x)
tolower(x)
toupper(x)
}
\keyword{character}
