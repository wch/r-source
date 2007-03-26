\name{sink}
\alias{sink}
\alias{sink.number}
\concept{tee}
\title{Send R Output to a File}
\usage{
sink(file = NULL, append = FALSE, type = c("output", "message"),
     split = FALSE)

sink.number(type = c("output", "message"))
}
\arguments{
  \item{file}{a connection or a character string naming the
    file to write to, or \code{NULL} to stop sink-ing.}
  \item{append}{logical. If \code{TRUE}, output will be appended to
    \code{file}; otherwise, it will overwrite the contents of
    \code{file}.}
  \item{type}{character.  Either the output stream or the messages
    stream.}
  \item{split}{logical: if \code{TRUE}, output will be sent to the new
    sink and to the current output stream, like the Unix program \code{tee}.}
}
\description{
  \code{sink} diverts \R output to a connection.

  \code{sink.number()} reports how many diversions are in use.

  \code{sink.number(type = "message")} reports the number of the
  connection currently being used for error messages.
}
\details{
  \code{sink} diverts \R output to a connection.  If \code{file} is a
  character string, a file connection with that name will be established
  for the duration of the diversion.

  Normal \R output (to connection \code{\link{stdout}))} is diverted by
  the default \code{type = "output"}.  Only prompts and (most)
  messages continue to appear on the console.  Messages sent to
  \code{\link{stdout}()} (including those from \code{\link{message}},
  \code{\link{warning}} and \code{\link{stop}}) can be diverted by
  \code{sink(type = "message")} (see below).

  \code{sink()} or \code{sink(file=NULL)} ends the last diversion (of
  the specified type).  There is a stack of diversions for normal
  output, so output reverts to the previous diversion (if there was
  one).  The stack is of up to 21 connections (20 diversions).

  If \code{file} is a connection it will be opened if necessary.

  Sink-ing the messages stream should be done only with great care.
  For that stream \code{file} must be an already open connection, and
  there is no stack of connections.
}
\value{
  \code{sink} returns \code{NULL}.

  For \code{sink.number()} the number (0, 1, 2, \dots) of diversions of
  output in place.

  For \code{sink.number("message")} the connection number used for
  messages, 2 if no diversion has been used.
}
\note{
  \code{sink(split = TRUE)} is only available on systems which support
  the C99 function \code{va_copy} (or under the name \code{__va_copy}),
  but we know of no current systems which do not.
}
\section{Warning}{
  Don't use a connection that is open for \code{sink} for any other
  purpose.  The software will stop you closing one such inadvertently.

  Do not sink the messages stream unless you understand the source code
  implementing it and hence the pitfalls.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer.
}
\seealso{\code{\link{capture.output}}}
\examples{
sink("sink-examp.txt")
i <- 1:10
outer(i, i, "*")
sink()
unlink("sink-examp.txt")
\dontrun{
## capture all the output to a file.
zz <- file("all.Rout", open="wt")
sink(zz)
sink(zz, type="message")
try(log("a"))
## back to the console
sink(type="message")
sink()
try(log("a"))
}}
\keyword{file}
\keyword{connection}
