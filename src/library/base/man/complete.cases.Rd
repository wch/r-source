\name{complete.cases}
\title{Find Complete Cases}
\usage{
complete.cases(\dots)
}
\alias{complete.cases}
\arguments{
\item{\dots}{a sequence of vectors, matrices and data frames.}
}
\value{
A logical vector specifying which
observations/rows have no missing values across the entire sequence.
}
\seealso{
\code{\link{is.na}}, \code{\link{na.omit}}, \code{\link{na.fail}}.
}
\examples{
data(airquality)
x <- airquality[, -1] # x is a regression design matrix
y <- airquality[,  1] # y is the corresponding response

all(!complete.cases(y) == is.na(y)) #--> TRUE
ok <- complete.cases(x,y)
sum(!ok) # how many are not "ok" ?
x <- x[ok,]
y <- y[ok]
}
\keyword{NA}
\keyword{logic}
