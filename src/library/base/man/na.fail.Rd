\name{na.fail}
\title{Handle Missing Values in Data Frames}
\usage{
na.fail(frame)
na.omit(frame)
}
\alias{na.fail}
\alias{na.omit}
\arguments{
  \item{frame}{a data frame}
}
\description{
  These functions are useful for dealing with \code{\link{NA}}s in data frames.
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
na.fail(DF)

options("na.action")
}
\keyword{NA}
