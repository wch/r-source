\name{dput}
\title{Write an Internal Object to a File}
\usage{
dput(x, file = "")
dget(file)
}
\alias{dput}
\alias{dget}
\value{
  \code{dput} opens \code{file} and deparses the object \code{x} into
  that file.  The object name is not written (contrary to \code{dump}).
  If \code{x} is a function the associated environment is stripped.
  Hence scoping information can be lost.

  Using \code{dget}, the object can be recreated (with the limitations
  mentioned above).
}
\seealso{
  \code{\link{deparse}}, \code{\link{dump}}, \code{\link{write}}.
}
\examples{
## Write an ASCII version of mean to the file "foo"
dput(mean, "foo")
## And read it back into `bar'
bar <- dget("foo")
}
\keyword{file}
\keyword{programming}
