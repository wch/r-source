\name{system}
\alias{system}
\alias{osx}
\title{Invoke a System X Command from Carbon R}
\description{
  \code{system} invokes the OS X shell commands specified by \code{command}.
}
\usage{
system(command,intern=FALSE)
}
\arguments{
  \item{command}{the system command to be invoked, as a string.}
  \item{intern}{a logical, indicates whether to make the output of the
    command an \R object.}
}
\value{
  If \code{intern=TRUE}, a character vector giving the output of the
  command, one line per character string. If the command could not be
  run or gives an error a \R error is generated.

  If \code{intern=FALSE}, the output of the command is sent directly to the
  \R Console.
}

\details{
  If \code{intern} is \code{TRUE} then the BSD \code{popen} function is used to 
  invoke the command and the output collected, line by line, into an \R
  \code{\link{character}} vector which is returned as the value of
  \code{system}.

  If \code{intern} is \code{FALSE} then the output of the command is
  redirected  directly to the  \R Console for a  maximum of 24k.
  
  This is an experimental and very limited code. It only works when Carbon R
  is running under MacOS X.
}
\examples{
# list all files in the current directory using the -al flag
system("ls -al")

t1 <- system("who",intern=TRUE)
t1

system("ls fizzlipuzzli")# empty since file doesn't exist

system("less AUTHORS") 
}
\keyword{interface}
\keyword{file}
\keyword{utilities}
