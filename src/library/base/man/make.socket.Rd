\name{make.socket}
\title{ Create a socket connection}
\usage{
make.socket(host="localhost", port, fail=T, server=F)
close.socket(socket)
}
\alias{make.socket}
%- Also NEED an `\alias' for EACH other function documented here.
\arguments{
 \item{host}{name of remote host}
 \item{port}{port to connect to/listen on }
 \item{fail}{Failure to connect is an error? }
 \item{server}{A server socket?}
}
\description{
 With \code{server=F} attempts to open a client socket to the specified
 port and host. With \code{server=T} listens on the specified port for a
 connection and then returns a server socket. It's a good idea to use
 \code{\link{on.exit}} to ensure that a socket is closed, as you only
 get 64 of them.
}
\value{
  An object of "class socket"
  \item{socket}{Socket number. This is for internal use}
  \item{port}{port number of the connection}
  \item{host}{name of remote computer}
}
\references{Adapted from Luke Tierney's code for XLISP-Stat, in turn
 based on code from Robbins and Robbins "Practical UNIX Programming"}
\section{WARNING }{I don't know if the connecting host name returned
  when \code{server=T} can be trusted. I suspect not.} 

\seealso{\code{\link{close.socket}},\code{\link{read.socket}},\code{\link{write.socket}} }

\examples{
daytime<-function(host="localhost"){
  a<-make.socket(host,13)
  on.exit(close.socket(a))
  read.socket(a)
}
daytime()  ## only works if your computer runs this service
daytime("chime.cac.washington.edu")
}
\keyword{ ~keyword }%-- one or more ...
