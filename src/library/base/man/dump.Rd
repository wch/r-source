\name{dump}
\title{Text Representations of R Objects}
\usage{
dump(list, fileout="dumpdata")
}
\alias{dump}
\description{
  This function takes a vector of names of \R objects and produces
  text representations of the objects in a file.
  This file, can be \code{\link{source}}d into another \R (or S) session.
}
\arguments{
  \item{list}{character. The names of one or more \R objects to be dumped.}
  \item{fileout}{character. The file name for the dump.}
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
unlink("xyz.Rdmped")
}
\keyword{file}
