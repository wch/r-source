\name{unlink}
\title{Delete Files and Directories}
\usage{unlink(x)}
\alias{unlink}
\arguments{
  \item{x}{a character vector with the names of the file(s) or
    directories to be deleted.
    Wildcards (normally \file{*} and \file{?}) are allowed.}
}
\description{
  \code{unlink} deletes the file(s) or directories specified by \code{x}.
}
\value{
#ifdef unix
  The return value of the corresponding system command, \code{rm -f},
  normally \code{0} for success, \code{1} for failure.
#endif
#ifdef windows
  \code{0} for success, \code{1} for failure.
#endif
  Not deleting a non-existent file is not a failure.
}
\seealso{\code{\link{tempfile}} for file creation.}
%Examples: tempfile has an 'unlink' example
\keyword{file}
