\name{which.min}
\alias{which.min}
\alias{which.max}
\title{Where is the Min() or Max() ?}
\description{
  Determines the location, i.e., index of the (first) minimum or maximum
  of a numeric vector.
}
\usage{
which.min(x)
which.max(x)
}
\arguments{
 \item{x}{numeric vector, whose \code{\link{min}} or \code{\link{max}}
   is searched.}
}
\value{
  an \code{\link{integer}} of length 1 or 0 (iff \code{x} has no
  non-\code{NA}s) , giving the index of the \emph{first} minimum or
  maximum respectively of \code{x}.

  If this extremum is unique (or empty), the result is the same (but more
  efficient) as  \code{which(x == min(x))} or \code{which(x == max(x))}
  respectively.
}
\author{Martin Maechler}
\seealso{\code{\link{which}}, \code{\link{max.col}}, \code{\link{max}}, etc.}
\examples{
x <- c(1:4,0:5,11)
which.min(x)
which.max(x)

data(presidents)
presidents[1:30]
range(presidents, na.rm = TRUE)
which.min(presidents)# 28
which.max(presidents)# 2
}
\keyword{utilities}
