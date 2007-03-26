\name{names}
\alias{names}
\alias{names.default}
\alias{names<-}
\alias{names<-.default}
\title{The Names of an Object}
\description{
  Functions to get or set the names of an object.
}
\usage{
names(x)
names(x) <- value
}
\arguments{
  \item{x}{an \R object.}
  \item{value}{a character vector of up to the same length as \code{x}, or
    \code{NULL}.} 
}
\value{
  For \code{names}, \code{NULL} or a character vector of the same length
  as \code{x}.  (\code{NULL} is given if the object has no names,
  including for objects of types which cannot have names.)

  For \code{names<-}, the updated object.  (Note that the value of
  \code{names(x) <- value} is that of the assignment, \code{value}, not
  the return value from the left-hand side.)
}
\details{
  \code{names} is a generic accessor function, and \code{names<-} is a
  generic replacement function.  The default methods get and set
  the \code{"names"} attribute of a vector (including a list) or
  pairlist.

  If \code{value} is shorter than \code{x}, it is extended by character
  \code{NA}s to the length of \code{x}.
  
  It is possible to update just part of the names attribute via the
  general rules: see the examples.  This works because the expression
  there is evaluated as \code{z <- "names<-"(z, "[<-"(names(z), 3, "c2"))}.

  The name \code{""} is special: it is used to indicate that there is no
  name associated with an element of a (atomic or generic) vector.
  Subscripting by \code{""} will match nothing (not even elements which
  have no name).

  A name can be character \code{NA}, but such a name will never be
  matched and is likely to lead to confusion.
}
\note{
  For vectors, the names are one of the \link{attributes} with
  restrictions on the possible values.  For pairlists, the names are the
  tags and converted to and from a character vector.

  For a one-dimensional array the \code{names} attribute really is
  \code{\link{dimnames}[[1]]}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
# print the names attribute of the islands data set
names(islands)

# remove the names attribute
names(islands) <- NULL
islands
rm(islands) # remove the copy made

z <- list(a=1, b="c", c=1:3)
names(z)
# change just the name of the third element.
names(z)[3] <- "c2"
z

z <- 1:3
names(z)
## assign just one name
names(z)[2] <- "b"
z
}
\keyword{attribute}
