\name{system}
\title{Invoke a System Command}
\usage{
system(command, intern = FALSE, ignore.stderr = FALSE)
}
\alias{system}
\alias{unix}
\arguments{
\item{command}{the system command to be invoked, as a string.}
\item{intern}{a logical, indicates whether to make the output of the
  command an \R object.}
\item{ignore.stderr}{a logical indicating whether error messages (written
  to \file{stderr}) should be ignored.  This probably only affects
  Unix-like systems and is ignored otherwise.}
}
\description{
  \code{system} invokes the system command specified by \code{command}.
}
\details{
  If \code{intern} is \code{TRUE} then \code{popen} is used to invoke the
  command and the output collected, line by line, into an \R
  \code{\link{character}} vector which is returned as the value of
  \code{system}.  If \code{intern}
  is \code{FALSE} then the C function \code{system} is used to invoke
  the command and the value returned by \code{system} is the exit
  status of this function.  This function provides users with
  the ability to invoke system commands on whatever platform they
  are using.

  \code{unix} is a \emph{deprecated} alternative, available for
  backwards compatibility.
}
\seealso{\code{\link{.Platform}} for platform specific variables.}
\examples{
# list all files in the current directory using the -F flag
\dontrun{system("ls -F")}

# t1 is a character vector, each one
# representing a separate line of output from who
t1 <- system("who", TRUE)

system("ls fizzlipuzzli",TRUE,TRUE)# empty since file doesn't exist
}
\keyword{interface}
\keyword{file}
\keyword{utilities}
