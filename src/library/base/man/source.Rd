\name{source}
\title{Redirect Input}
\usage{
source(file, local=FALSE)
}
\alias{source}
\arguments{
\item{file}{the name of the file to read from (quoted).}
\item{local}{if \code{local} is \code{FALSE},
the statements scanned are evaluated
in the global environment, otherwise in the local calling
source.}
}
\description{
\code{source} causes R to accept its input from the named
file (the name must be quoted).
Input is read from that file until the
end of the file is reached.
\code{\link{parse}} is used to scan the files in, they are then evaluated
sequentially in the chosen environment.
}
