\name{system}
\title{Invoke a System Command}
\usage{
system(command, intern=FALSE)
}
\alias{system}
\arguments{
\item{command}{the system command to be invoked, as a string.}
\item{intern}{a logical, indicates whether to make the output of the
command an \R object.}
}
\description{
\code{system} invokes the system command specified by \code{command}.
If \code{intern} is \code{TRUE} then popen is used to invoke the
command and the output collected, line by line, into an \R string
vector which is returned as the value of \code{system}.  If \code{intern}
is \code{FALSE} then the C function \code{system} is used to invoke
the command and the value returned by \code{system} is the exit
status of this function.  This function provides users with
the ability to invoke system commands on whatever platform they
are using.
}
\examples{
# list all files in the current directory
# using the -F flag
system("ls -F")

# t1 contains a vector of strings, each one
# representing a separate line of output from who
t1 <- system("who", TRUE)
}
\keyword{interface}
\keyword{file}
\keyword{utilities}
