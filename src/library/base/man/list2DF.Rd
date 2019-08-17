\name{list2DF}
\alias{list2DF}
\title{
  Create Data Frame From List
}
\description{
  Create a data frame from a list of variables.
}
\usage{
list2DF(x = list(), nrow = NULL)
}
\arguments{
  \item{x}{
    A list of variables for the data frame.
  }
  \item{nrow}{
    An integer giving the desired number of rows for the data frame, or
    \code{NULL} (default), in which case the maximal length of the
    elements of the list will be used.  If necessary, list elements will
    be replicated to the same length given by the number of rows.
  }
}
\details{
  Note that all list elements are taken \dQuote{as is} (apart from
  possibly replicating to the same length).
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
