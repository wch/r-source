\name{Encoding}
\encoding{latin1}
\alias{Encoding}
\alias{Encoding<-}
\title{Read or Set the Declared Encodings for a Character Vector}
\description{
  Read or set the declared encodings for a character vector.
}
\usage{
Encoding(x)

Encoding(x) <- value
}
\arguments{
  \item{x}{A character vector.}
  \item{value}{A character vector of positive length.}
}
\details{
  As from \R 2.5.0, character strings in \R can be declared to be in
  \code{"latin1"} or \code{"UTF-8"}.  These declarations can be read by
  \code{Encoding}, which will return a character vector of values
  \code{"latin1"}, \code{"UTF-8"} or \code{"unknown"}, or set, when
  \code{value} is recycled as needed and other values are silently
  treated as \code{"unknown"}.

  There are other ways for character strings to acquire a declared
  encoding apart from explicitly setting it.  Functions
  \code{\link{scan}}, \code{\link{read.table}}, \code{\link{readLines}},
  \code{\link{parse}} and  \code{\link{source}} have an \code{encoding}
  argument that is used to declare encodings, \code{\link{iconv}}
  declares encodings from its \code{from} argument, and console input in
  suitable locales is also declared.
}
\value{
  A character vector.
}
\examples{
## x is intended to be in latin1
x <- "fa\xE7ile"
Encoding(x)
Encoding(x) <- "latin1"
x
if(capabilities("iconv")) {
  xx <- iconv(x, "latin1", "UTF-8")
  print(Encoding(c(x, xx)))
  c(x, xx)
}
}
\keyword{utilities}
\keyword{character}
