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
  \item{old}{a character string specifying the characters to be
    translated.}
  \item{new}{a character string specifying the translations.}
}
\details{
  \code{chartr} translates each character in \code{x} that is specified
  in \code{old} to the corresponding character specified in \code{new}.
  Ranges are supported in the specifications, but character classes and
  repreated characters are not.  If \code{old} contains more characters
  than new, an error is signaled; if it contains fewer characters, the
  extra characters at the end of \code{new} are ignored.  

  \code{tolower} and \code{toupper} convert upper-case characters in a
  character vector to lower-case, or vice versa.  Non-alphabetic
  characters are left unchanged.
}
\examples{
x <- "MiXeD cAsE 123"
chartr("iXs", "why", x)
chartr("a-cX", "D-Fw", x)
tolower(x)
toupper(x)
}
\keyword{character}
