\name{dump}
\title{Text Representations of R Objects}
\usage{
dump(list, file="dumpdata.R", append=FALSE)
}
\alias{dump}
\description{
  This function takes a vector of names of \R objects and produces
  text representations of the objects on a file or connection.
  A \code{dump} file can be \code{\link{source}}d into another
  \R (or S) session.
}
\arguments{
  \item{list}{character. The names of one or more \R objects to be dumped.}

  \item{file}{either a character string naming a file or a
    connection. \code{""} indicates output to the console.}

  \item{append}{if \code{TRUE}, output will be appended to
    \code{file}; otherwise, it will overwrite the contents of
    \code{file}.}
}
\details{
  At present the implementation of \code{dump} is very
  incomplete and it really only works for functions and
  simple vectors.

  \code{dump} will warn if fewer characters were written to a file than
  expected, which may indicate a full or corrupt file system.

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
\keyword{connection}
