\name{capture.output}
\alias{capture.output}
\title{Send output to a character string or file}
\description{
  Evaluates its arguments with the output being returned as a character
  string or sent to a file. Related to \code{\link{sink}} in the same
  way that \code{\link{with}} is related to \code{\link{attach}}.
}
\usage{
capture.output(\dots, file = NULL, append = FALSE)
}
\arguments{
  \item{\dots}{Expressions to be evaluated}
  \item{file}{A file name or a connection, or \code{NULL} to return
    the output as a string. If the connection is not open it will be
    opened and then closed on exit.}
  \item{append}{Append or overwrite the file? }
}
\value{
 A character string, or \code{NULL} if a \code{file} argument was
 supplied.  
}

\seealso{ \code{\link{sink}}, \code{\link{textConnection}} }

\examples{
require(stats)
glmout <- capture.output(example(glm))
glmout[1:5]
capture.output(1+1, 2+2)
capture.output({1+1; 2+2})
\dontrun{
## on Unix with enscript available
ps <- pipe("enscript -o tempout.ps","w")
capture.output(example(glm), file=ps)
close(ps)
}
}
\keyword{utilities}
