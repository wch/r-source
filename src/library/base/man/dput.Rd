\name{dput}
\alias{dput}
\alias{dget}
\title{Write an Internal Object to a File}
\description{
  Writes an ASCII text representation of an \R object to a file
  or connection, or uses one to recreate the object.
}
\usage{
dput(x, file = "", fordisplay = TRUE)
dget(file)
}
\arguments{
  \item{x}{an object.}
  \item{file}{either a character string naming a file or a
    connection. \code{""} indicates output to the console.}
  \item{fordisplay}{logical indicating whether to make the representation 
                    more readable}
}
\details{
  \code{dput} opens \code{file} and deparses the object \code{x} into
  that file.  The object name is not written (contrary to \code{dump}).
  If \code{x} is a function the associated environment is stripped.
  Hence scoping information can be lost.

  Deparsing an object is difficult, and not always possible.  With the
  default \code{fordisplay = TRUE}, \code{dput()} attempts to deparse in
  a way that is readable, but for more complex or unusual objects, not
  likely to be parsed as identical to the original.  For example,
  attributes are usually not shown.  The argument 
  \code{fordisplay = FALSE} causes the deparsing to be more faithful to
  the original, so this option should usually be used when \code{dget} is
  planned:  but even in this case, some objects will not be reproduced
  identically.

  \code{dput} will warn if fewer characters were written to a file than
  expected, which may indicate a full or corrupt file system.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{deparse}}, \code{\link{dump}}, \code{\link{write}}.
}
\examples{
## Write an ASCII version of mean to the file "foo"
dput(mean, "foo")
## And read it back into 'bar'
bar <- dget("foo")
unlink("foo")
}
\keyword{file}
\keyword{programming}
\keyword{connection}
