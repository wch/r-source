\name{names}
\alias{names}
\alias{names.default}
\alias{names<-}
\alias{names<-.default}
\title{The Names Attribute of an Object}
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
  as \code{x}.

  For \code{names<-}, the updated object.  (Note that the value of
  \code{names(x) <- value} is that of the assignment, \code{value}, not
  the return value from the left-hand side.)
}
\details{
  \code{names} is a generic accessor function, and \code{names<-} is a
  generic replacement function.  The default methods get and set
  the \code{"names"} attribute of a vector or list.

  If \code{value} is shorter than \code{x}, it is extended by character
  \code{NA}s to the length of \code{x}.
  
  It is possible to update just part of the names attribute via the
  general rules: see the examples.  This works because the expression
  there is evaluated as \code{z <- "names<-"(z, "[<-"(names(z), 3, "c2"))}.
}
\examples{
data(islands)
# print the names attribute of the islands data set
names(islands)

# remove the names attribute
names(islands) <- NULL

z <- list(a=1, b="c", c=1:3)
names(z)
# change just the name of the third element.
names(z)[3] <- "c2"
z

## assign just one name
z <- 1:3
names(z)
# change just the name of the third element.
names(z)[2] <- "b"
z
}
\keyword{attribute}
