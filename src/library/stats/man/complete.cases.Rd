% File src/library/stats/man/complete.cases.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{complete.cases}
\alias{complete.cases}
\title{Find Complete Cases}
\description{
  Return a logical vector indicating which cases are complete, i.e.,
  have no missing values.
}
\usage{
complete.cases(\dots)
}
\arguments{
  \item{\dots}{a sequence of vectors, matrices and data frames.}
}
\value{
  A logical vector specifying which observations/rows have no missing
  values across the entire sequence.
}
\seealso{
  \code{\link{is.na}},
  \code{\link{na.omit}},
  \code{\link{na.fail}}.
}
\examples{
x <- airquality[, -1] # x is a regression design matrix
y <- airquality[,  1] # y is the corresponding response

stopifnot(complete.cases(y) != is.na(y))
ok <- complete.cases(x, y)
sum(!ok) # how many are not "ok" ?
x <- x[ok,]
y <- y[ok]
}
\keyword{NA}
\keyword{logic}
