\name{sink}
\title{Send \R Output to a File}
\usage{
sink(file = NULL)
}
\alias{sink}
\arguments{
  \item{file}{a character string naming the file to write to, or
    \code{NULL} to stop ``sink''ing.}
}
\description{
  \code{sink(file)} diverts all \R output to \code{file} (overwriting the
  file if it already exists;  appending is not supported yet).  Only
  prompts and error messages continue to appear on the terminal.

  \code{sink()} or \code{sink(file=NULL)} ends the diversion.
}
\examples{
sink("sink-examp.txt")
i <- 1:10
outer(i,i,"*")
sink()
}
\keyword{file}
