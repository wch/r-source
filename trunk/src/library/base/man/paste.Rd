\name{paste}
\title{Concatenate Strings}
\concept{combine strings}
\usage{
paste(\dots, sep = " ", collapse = NULL)
}
\alias{paste}
\arguments{
  \item{\dots}{one or more \R objects, to be coerced to character vectors.}
  \item{sep}{a character string to separate the terms.}
  \item{collapse}{an optional character string to separate the results.}
}
\description{
  Concatenate vectors after converting to character.
}
\details{
  \code{paste} converts its arguments to character strings, and
  concatenates them (separating them by the string given by \code{sep}).
  If the arguments are vectors, they are concatenated term-by-term to
  give a character vector result.

  If a value is specified for \code{collapse}, the values in the result
  are then concatenated into a single string, with the elements being
  separated by the value of \code{collapse}.
}
\value{
  A character vector of the concatenated values.  This will be of length
  zero if all the objects are, unless \code{collapse} is non-NULL, in
  which case it is a single empty string.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  String manipulation with
  \code{\link{as.character}}, \code{\link{substr}}, \code{\link{nchar}},
  \code{\link{strsplit}}; further, \code{\link{cat}} which concatenates and
  writes to a file, and \code{\link{sprintf}} for C like string
  construction.
}
\examples{
paste(1:12) # same as as.character(1:12)
paste("A", 1:6, sep = "")
paste("Today is", date())
}
\keyword{character}
