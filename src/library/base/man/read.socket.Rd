\name{read.socket}
\title{ Read from or write to a socket}
\usage{
read.socket(socket, maxlen=256, loop=F)
write.socket(socket, string)
}
%- maybe also `usage' for other functions documented here.
\alias{read.socket}
\alias{write.socket}
\arguments{
 \item{socket}{A socket object}
 \item{maxlen}{Maximum length of string to read }
 \item{loop}{Wait for ever if there is nothing to read?}
 \item{string]{String to write to socket}
}
\description{
\code{read.socket} reads a string from the specified socket,
\code{write.socket} writes to the specified socket.  There is very
little error checking in either.
}
\value{
  \code{read.socket} returns the string read.
}

\seealso{\code{\link{close.socket}},\code{\link{make.socket}} }

\examples{
finger<-function(user,host="localhost", port=79,print=T){
  if (!is.character(user))
    stop("user name must be a string")
  user<-paste(user,"\r\n")
  socket<-make.socket(host,port)
  on.exit(close.socket(socket))
  write.socket(socket,user)
  output<-character(0)
  repeat{
    ss<-read.socket(socket)
    if (ss=="")
      break
    output<-paste(output,ss)
  }
  close.socket(socket)
  if (print) cat(output)
  invisible(output)
}
finger("root")  ## only works if your site provides a finger daemon
}
\keyword{ ~keyword }%-- one or more ...
