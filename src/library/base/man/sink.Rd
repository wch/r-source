\name{sink}
\alias{sink}
\title{Send R Output to a File}
\usage{
sink(file = NULL, append = FALSE)
}
\arguments{
  \item{file}{a connection or a character string naming the
    file to write to, or \code{NULL} to stop ``sink''ing.}
  \item{append}{if \code{TRUE}, output will be appended to
    \code{file}; otherwise, it will overwrite the contents of
    \code{file}.}
}
\description{
    Diverts all \R output to \code{file}, overwriting the file unless
    \code{append} is \code{TRUE}.
}
\details{
    Only prompts and error messages continue to appear on the terminal.

    \code{sink()} or \code{sink(file=NULL)} ends the diversion.

    If \code{file} is a connection if will be opened if necessary.
    Switching to another file or connection closes and destroys the
    current sink connection if it is a file connection opened by the
    last call to \code{sink}. 
}
\examples{
sink("sink-examp.txt")
i <- 1:10
outer(i,i,"*")
sink()
unlink("sink-examp.txt")
}
\keyword{file}
\keyword{connection}
