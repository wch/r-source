\name{unlink}
\title{Delete Files}
\usage{
unlink(x)
}
\alias{unlink}
\arguments{
    \item{x}{a character vector with the name of the files to be deleted.
	Wildcards (\file{*} and \file{?}) are allowed.}
}
\description{
    \code{unlink} deletes the file(s) specified by \code{x}.
}
\value{
    \code{0} for success, \code{1} for failure.
}
\seealso{\code{\link{tempfile}} for file creation.}
\keyword{file}
