\name{dput}
\alias{dput}
\alias{dget}
\title{Write an Internal Object to a File}
\description{
  Writes an ASCII text representation of an \R object to a file
  or connection, or uses one to recreate the object.
}
\usage{
dput(x, file = "", forDisplay = TRUE, useSource = FALSE)
dget(file)
}
\arguments{
  \item{x}{an object.}
  \item{file}{either a character string naming a file or a
    connection. \code{""} indicates output to the console.}
  \item{forDisplay}{logical indicating whether to make the representation 
                    more readable}
  \item{useSource}{logical indicating whether to use the saved source
  		   when writing a function}
}
\details{
  \code{dput} opens \code{file} and deparses the object \code{x} into
  that file.  The object name is not written (contrary to \code{dump}).
  If \code{x} is a function the associated environment is stripped.
  Hence scoping information can be lost.

  Deparsing an object is difficult, and not always possible.  With the
  default \code{forDisplay = TRUE}, \code{dput()} attempts to deparse in
  a way that is readable, but for more complex or unusual objects, not
  likely to be parsed as identical to the original.  For example,
  attributes are usually not shown.  The argument 
  \code{forDisplay = FALSE} causes the deparsing to be more faithful to
  the original, so this option should usually be used when \code{dget} is
  planned:  but even in this case, some objects will not be reproduced
  identically.

  \code{dput} will warn if fewer characters were written to a file than
  expected, which may indicate a full or corrupt file system.
    
  To display saved source rather than deparsing the internal representation 
  use \code{useSource = TRUE}.  \R currently saves source only for
  function definitions.
}
\note{
  To avoid the risk of a source attribute out of sync with the actual
  function definition, the source attribute of a function will never 
  be written as an attribute.
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
## Create a function with comments
baz <- function(x) {
  # Subtract from one
  1-x
}
## and display it
dput(baz)
## and now display the saved source
dput(baz, useSource = TRUE)
}
\keyword{file}
\keyword{programming}
\keyword{connection}
