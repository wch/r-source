\name{object.size}
\alias{object.size}
\title{Report the Space Allocated for an Object}
\description{
  Provides an estimate of the memory that is being used to store an \R object.
}
\usage{
object.size(x)
}
\arguments{
  \item{x}{An \R object.}
}
\details{
  Exactly which parts of the memory allocation should be attributed
  to which object is not clear-cut.  This function merely provides
  a rough indication.  For example, it will not detect if character
  storage for character strings are shared between identical elements
  (which it will be if \code{rep} was used, for example).

  The calculation is of the size of the object, and excludes the space
  needed to store its name in the symbol table.

}
\value{
  An estimate of the memory allocation attributable to the object, in bytes.
}

\examples{
object.size(letters)
object.size(ls)
## find the 10 largest objects in base
z <- sapply(ls("package:base"), function(x) object.size(get(x)))
as.matrix(rev(sort(z))[1:10])
}
\keyword{utilities}
