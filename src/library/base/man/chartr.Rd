\name{chartr}
\alias{chartr}
\alias{tolower}
\alias{toupper}
\title{Character Translation}
\description{
  Translate characters in character vectors.
}
\usage{
chartr(old, new, x)
tolower(x)
toupper(x)
}
\arguments{
  \item{x}{a character vector.}
  \item{old}{a character string containing the characters to be
    translated.}
  \item{new}{a character string containing the translations.}
}
\details{
  \code{chartr} translates each character in \code{x} that is contained
  in \code{old} to the corresponding character in \code{new}.  If
  \code{old} contains more characters than new, an error is signaled; if
  it contains fewer characters, the extra characters at the end of
  \code{new} are ignored.  Ranges or character classes are not
  supported.

  \code{tolower} and \code{toupper} convert upper-case characters in a
  character vector to lower-case, or vice versa.  Non-alphabetic
  characters are left unchanged.
}
\examples{
x <- "MiXeD cAsE 123"
chartr("iXs", "why", x)
tolower(x)
toupper(x)
}
\keyword{character}
