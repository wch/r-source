\name{dump}
\title{Text Representations of R Objects}
\usage{
dump(list, fileout="dumpdata")
}
\alias{dump}
\description{
  This function takes a vector of names of \R objects and produces
  text representations of the objects in the file \code{fileout}.
  This file, can be \code{\link{source}}d into another \R (or S) session.
}
\details{
  At present the implementation of \code{dump} is very
  incomplete and it really only works for functions and
  simple vectors.

  The function \code{\link{save}} is designed to be used for
  transporting \R data between machines.
}
\seealso{\code{\link{dput}}, \code{\link{dget}},\code{\link{write}}.
}
\examples{
x <- 1; y <- 1:10
dump(ls(patt='^[xyz]'), "xyz.Rdmped")
}
\keyword{file}
