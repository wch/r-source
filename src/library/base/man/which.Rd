\name{which}
\title{Which indices are TRUE ?}
\usage{
which(x)
}
\alias{which}
\arguments{
 \item{x}{a logical vector or array.}
}
\value{
  An integer vector with \code{length} equal to \code{sum(x)}, i.e. to
  the number of \code{TRUE}s in \code{x};
  Basically, the result is \code{(1:length(x))[x]}.
}
\seealso{\code{\link{Logic}}}

\examples{
which(LETTERS == "R")
which((1:12)\%\%2 == 0) #which are even?
}
\keyword{logic}
\keyword{attribute}
