\name{head}
\alias{head}
\alias{head.default}
\alias{head.data.frame}
\alias{head.function}
\alias{head.matrix}
\alias{tail}
\alias{tail.default}
\alias{tail.data.frame}
\alias{tail.function}
\alias{tail.matrix}

\title{
  Return the First or Last Part of an Object
}
\description{
  Returns the first or last parts of a vector, matrix, data frame or function.
}
\usage{
head(x, \dots)
\method{head}{default}(x, n=6, \dots)
\method{head}{data.frame}(x, n=6, \dots)

tail(x, \dots)
\method{tail}{default}(x, n=6, \dots)
\method{tail}{data.frame}(x, n=6, \dots)
}
\arguments{
  \item{x}{an object}
  \item{n}{size for the resulting object: number of elements for a
    vector (including lists), rows for a matrix or data frame or
    lines for a function.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\details{
  For matrices and data frames, the first/last \code{n} rows are returned.
  For functions, the first/last \code{n} lines of the deparsed function are
  returned as character strings.
}
\value{
  An object (usually) like \code{x} but generally smaller.
}
\author{
  Patrick Burns, improved and corrected by R-core
}

\examples{
data(freeny)
head(freeny.x, n=10)
head(freeny.y)

tail(freeny.x)
tail(freeny.y)

tail(library)
}
\keyword{ manip }
