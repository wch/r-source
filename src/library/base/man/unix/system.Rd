\name{system}
\alias{system}
\alias{unix}
\title{Invoke a System Command}
\description{
  \code{system} invokes the OS command specified by \code{command}.
}
\usage{
system(command, intern = FALSE, ignore.stderr = FALSE)
}
\arguments{
  \item{command}{the system command to be invoked, as a string.}
  \item{intern}{a logical, indicates whether to make the output of the
    command an \R object.  Not available unless \code{popen} is
    supported on the platform.}
  \item{ignore.stderr}{a logical indicating whether error messages (written
    to \file{stderr}) should be ignored.}
}
\value{
  If \code{intern=TRUE}, a character vector giving the output of the
  command, one line per character string. If the command could not be
  run or gives an error a \R error is generated.

  If \code{intern=FALSE}, the return value is an error code.
}
\details{
  If \code{intern} is \code{TRUE} then \code{popen} is used to invoke the
  command and the output collected, line by line, into an \R
  \code{\link{character}} vector which is returned as the value of
  \code{system}.  Output lines of more than 8095 characters will be split.

  If \code{intern} is \code{FALSE} then the C function \code{system}
  is used to invoke the command and the value returned by \code{system}
  is the exit status of this function.

  \code{unix} is a \emph{deprecated} alternative, available for
  backwards compatibility.
}
\seealso{\code{\link{.Platform}} for platform specific variables.}
\examples{
# list all files in the current directory using the -F flag
\dontrun{system("ls -F")}

# t1 is a character vector, each one
# representing a separate line of output from who
# (if the platform has popen and who)
t1 <- try(system("who", TRUE))

try(system("ls fizzlipuzzli", TRUE, TRUE))
# empty since file doesn't exist
}
\keyword{interface}
\keyword{file}
\keyword{utilities}
