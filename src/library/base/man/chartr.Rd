\name{chartr}
\alias{tolower}
\alias{toupper}
\title{Character Translation}
\description{
  Convert upper-case characters in a character vector to lower-case, or
  vice versa.  Non-alphabetic characters are left unchanged.
}
\usage{
tolower(x)
toupper(x)
}
\arguments{
  \item{x}{a character vector.}
}
\examples{
x <- "MiXeD cAsE 123"
tolower(x)
toupper(x)
}
\keyword{character}
