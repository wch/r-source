\name{sink}
\title{Send \R Output to a File}
\usage{
sink()
sink(file)
}
\alias{sink}
\arguments{
  \item{file}{a character string naming the file to write to.}
}
\description{
  \code{sink(file)} diverts all \R output to \code{file} (overwriting the
  file if it already exists;  appending is not supported yet).  Only
  prompts and error messages continue to appear on the terminal.

  \code{sink()} ends the diversion.
}
\keyword{file}
