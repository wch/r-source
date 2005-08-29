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
  Exactly which parts of the memory allocation should be attributed to
  which object is not clear-cut.  This function merely provides a rough
  indication: it should be reasonably accurate for atomic vectors, but
  does not detect if elements of a list are shared, for example.
  (Sharing amongst elements of a character vector is taken into account.)
  
  The calculation is of the size of the object, and excludes the space
  needed to store its name in the symbol table.

  Associated space (e.g. the enviromment of a function and what the
  pointer in a \code{EXTPTRSXP} points to) is not included in the
  calculation.

  Object sizes are larger on 64-bit platforms than 32-bit ones, but will
  very likely be the same on different platforms with the same word length.
}
\value{
  An estimate of the memory allocation attributable to the object, in bytes.
}
\seealso{
  \code{\link{Memory-limits}} for the design limitations on object size.
}

\examples{
object.size(letters)
object.size(ls)
## find the 10 largest objects in the base package
z <- sapply(ls("package:base"), function(x) object.size(get(x, envir=baseenv())))
as.matrix(rev(sort(z))[1:10])
}
\keyword{utilities}
