\name{na.fail}
\title{Handle Missing Values in Objects}
\usage{
na.fail(object)
na.omit(object)
}
\alias{na.fail}
\alias{na.fail.default}
\alias{na.omit}
\alias{na.omit.default}
\arguments{
  \item{object}{an \R object, typically a data frame}
}
\description{
  These generic functions are useful for dealing with \code{\link{NA}}s
  in e.g., data frames.
  \code{na.fail} returns the data frame if it does not contain any
  missing values, and signals an error otherwise.
  \code{na.omit} returns the data frame with incomplete cases removed.
}
\seealso{
  \code{\link{na.action}}, \code{\link{options}(na.action=..)} for setting ``NA
  actions'', and \code{\link{lm}} and \code{\link{glm}} for functions
  using these.
}
\examples{
DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
na.omit(DF)
\dontrun{na.fail(DF)#> Error: missing values in ..}

options("na.action")
}
\keyword{NA}
