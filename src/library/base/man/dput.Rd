\name{dput}
\title{Write an Internal Object to a File}
\usage{
dput(x, file="")
}
\alias{dput}
\value{
\code{dput} opens \code{file} and deparses the object \code{x} into that file.
If \code{x} is a function the associated environment is stripped.
Hence scoping information can be lost.
}
\seealso{
\code{\link{deparse}}, \code{\link{dump}}, \code{\link{dget}}, \code{\link{write}}.
}
\examples{
# write an ASCII version of mean to the file "foo"
dput(mean,"foo")
}
