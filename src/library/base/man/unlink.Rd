\name{unlink}
\title{Delete Files}
\usage{unlink(x)}
\alias{unlink}
\arguments{
\item{x}{a character vector with the name of the files to be deleted.
Shell wildcards are allowed.}}
\description{
\code{unlink} deletes the files specified by \code{x}.
}
\value{The return value of the corresponding system command.}
\seealso{\code{\link{tempfile}} for file creation.}
%Examples: tempfile has an 'unlink' example
\keyword{file}
