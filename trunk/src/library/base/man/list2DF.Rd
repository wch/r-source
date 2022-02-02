\name{list2DF}
\alias{list2DF}
\title{
  Create Data Frame From List
}
\description{
  Create a data frame from a list of variables.
}
\usage{
list2DF(x = list(), nrow = 0)
}
\arguments{
  \item{x}{
    A list of same-length variables for the data frame.
  }
  \item{nrow}{
    An integer giving the desired number of rows for the data frame in
    case \code{x} gives no variables (i.e., has length zero).
  }
}
\details{
  Note that all list elements are taken \dQuote{as is}.
}
\value{
  A data frame with the given variables.
}
\seealso{
  \code{\link{data.frame}}
}
\examples{
## Create a data frame holding a list of character vectors and the
## corresponding lengths:
x <- list(character(), "A", c("B", "C"))
n <- lengths(x)
list2DF(list(x = x, n = n))

## Create data frames with no variables and the desired number of rows:
list2DF()
list2DF(nrow = 3L)
}
\keyword{classes}
