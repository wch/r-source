\name{embed}
\title{Embedding a Time Series}
\usage{
embed (x, dimension = 1) 
}
\alias{embed}
\arguments{
  \item{x}{a numeric vector, matrix, or time series.}
  \item{dimension}{a scalar representing the embedding dimension.}
}
\description{
  Embeds the time series \code{x} into a low-dimensional
  Euclidean space.
}
\details{
  Each row of the resulting matrix consists of sequences
  \code{x[t]}, \code{x[t-1]}, \dots, \code{x[t-dimension+1]}, where
  \code{t} is the original index of \code{x}. If \code{x} is a matrix,
  i.e., \code{x} contains more than one variable, then \code{x[t]}
  consists of the \code{t}th observation on each variable.
}
\value{
  A matrix containing the embedded time series \code{x}.
}
\author{A. Trapletti, B.D. Ripley}
\examples{
x <- 1:10
embed (x, 3)
}
\keyword{ts}

