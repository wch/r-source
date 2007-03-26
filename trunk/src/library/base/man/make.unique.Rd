\name{make.unique}
\alias{make.unique}
\title{Make Character Strings Unique}
\description{
  Makes the elements of a character vector unique by
  appending sequence numbers to duplicates.
}
\usage{
make.unique(names, sep = ".")
}
\arguments{
  \item{names}{a character vector}
  \item{sep}{a character string used to separate a duplicate name from
    its sequence number.}
}
\value{
  A character vector of same length as \code{names} with duplicates changed.
}
\details{
  The algorithm used by \code{make.unique} has the property that
  \code{make.unique(c(A, B)) == make.unique(c(make.unique(A), B))}.

  In other words, you can append one string at a time to a vector,
  making it unique each time, and get the same result as applying
  \code{make.unique} to all of the strings at once.
    
  If character vector \code{A} is already unique, then
  \code{make.unique(c(A, B))} preserves \code{A}.
}
\author{Thomas P Minka}
\seealso{
  \code{\link{make.names}}
}
\examples{
make.unique(c("a", "a", "a"))
make.unique(c(make.unique(c("a", "a")), "a"))

make.unique(c("a", "a", "a.2", "a"))
make.unique(c(make.unique(c("a", "a")), "a.2", "a"))

rbind(data.frame(x=1), data.frame(x=2), data.frame(x=3))
rbind(rbind(data.frame(x=1), data.frame(x=2)), data.frame(x=3))
}
\keyword{character}
