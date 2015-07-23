% File src/library/utils/man/object.size.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{object.size}
\alias{object.size}
\alias{format.object_size}
\alias{print.object_size}
\title{Report the Space Allocated for an Object}
\description{
  Provides an estimate of the memory that is being used to store an \R object.
}
\usage{
object.size(x)

\method{format}{object_size}(x, units = "b", \dots)
\method{print}{object_size}(x, quote = FALSE, units = "b", \dots)
}
\arguments{
  \item{x}{An \R object.}
  \item{quote}{logical, indicating whether or not the result should be
    printed with surrounding quotes.}
  \item{units}{The units to be used in printing the size.  Allowed
    values are \code{"b"}, \code{"Kb"}, \code{"Mb"}, \code{"Gb"},
    \code{"B"}, \code{"KB"}, \code{"MB"}, \code{"GB"}, and \code{"auto"}
    (see \sQuote{Details}).} 
  \item{\dots}{Arguments to be passed to or from other methods.}
}
\details{
  Exactly which parts of the memory allocation should be attributed to
  which object is not clear-cut.  This function merely provides a rough
  indication: it should be reasonably accurate for atomic vectors, but
  does not detect if elements of a list are shared, for example.
  (Sharing amongst elements of a character vector is taken into account,
  but not that between character vectors in a single object.)

  The calculation is of the size of the object, and excludes the space
  needed to store its name in the symbol table.

  Associated space (e.g., the environment of a function and what the
  pointer in a \code{EXTPTRSXP} points to) is not included in the
  calculation.

  Object sizes are larger on 64-bit builds than 32-bit ones, but will
  very likely be the same on different platforms with the same word
  length and pointer size.

  \code{units = "auto"} in the \code{format} and \code{print} methods
  chooses the largest units in which the result is one or more (before
  rounding).  Values in kilobytes, megabytes or gigabytes are rounded to
  the nearest \code{0.1}.
}
\value{
  An object of class \code{"object_size"} with a length-one double value,
  an estimate of the memory allocation attributable to the object in bytes.
}
\seealso{
  \code{\link{Memory-limits}} for the design limitations on object size.
}

\examples{
object.size(letters)
object.size(ls)
format(object.size(library), units = "auto")
## find the 10 largest objects in the base package
z <- sapply(ls("package:base"), function(x)
            object.size(get(x, envir = baseenv())))
as.matrix(rev(sort(z))[1:10])
}
\keyword{utilities}
