\name{dput}
\alias{dput}
\alias{dget}
\title{Write an Internal Object to a File}
\description{
  Writes an ASCII text representation of an \R object to a file
  or connection, or uses one to recreate the object.
}
\usage{
dput(x, file = "")
dget(file)
}
\arguments{
  \item{x}{an object.}
  \item{file}{either a character string naming a file or a
    connection. \code{""} indicates output to the console.}
}
\details{
  \code{dput} opens \code{file} and deparses the object \code{x} into
  that file.  The object name is not written (contrary to \code{dump}).
  If \code{x} is a function the associated environment is stripped.
  Hence scoping information can be lost.

  Using \code{dget}, the object can be recreated (with the limitations
  mentioned above).

  \code{dput} will warn if fewer characters were written to a file than
  expected, which may indicate a full or corrupt file system.
}
\seealso{
  \code{\link{deparse}}, \code{\link{dump}}, \code{\link{write}}.
}
\examples{
## Write an ASCII version of mean to the file "foo"
dput(mean, "foo")
## And read it back into `bar'
bar <- dget("foo")
unlink("foo")
}
\keyword{file}
\keyword{programming}
\keyword{connection}
