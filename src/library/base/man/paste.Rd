\name{paste}
\title{Concatenate Strings}
\usage{
paste(\dots, sep = " ", collapse = NULL)
}
\alias{paste}
\description{
  \code{paste} converts its arguments to character strings, and
  concatenates them (separating them by the string given by \code{sep}).
  If a value is specified for \code{collapse}, the values in the result
  are then concatenated into a single string, with the elements being
  separated by the value of \code{collapse}.

  The concatenated value is returned as the value of \code{paste}.
}
\seealso{
  \code{\link{as.character}}, \code{\link{substr}}, \code{\link{nchar}},
  \code{\link{strsplit}}.
}
\examples{
paste(1:12) # same as as.character(..)
paste("A", 1:6, sep = "")
paste("Today is", date())
}
\keyword{character}
