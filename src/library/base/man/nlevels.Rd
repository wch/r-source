\name{nlevels}
\title{The Number of Levels of a Factor}
\usage{
nlevels(x)
}
\alias{nlevels}
\description{
  Return the number of levels which its argument has.
}
\arguments{
  \item{x}{an object, usually a factor.} 
}
\details{
  This is usually applied to a factor, but other objects can have levels.
  
  The actual factor levels (if they exist) can be obtained
  with the \code{\link{levels}} function.
}
\value{
  The length of \code{\link{levels}(x)}, which is zero if
  \code{x} has no levels.
}
\seealso{
  \code{\link{levels}}, \code{\link{factor}}.
}
\examples{
nlevels(gl(3,7)) # = 3
}
\keyword{category}
