% File src/library/graphics/man/plot.dataframe.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{plot.data.frame}
\alias{plot.data.frame}
\title{Plot Method for Data Frames}
\description{
  \code{plot.data.frame}, a method for the \code{\link{plot}} generic.
  It is designed for a quick look at numeric data frames.
}
\usage{
\method{plot}{data.frame}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{data.frame}.}
  \item{\dots}{further arguments to \code{\link{stripchart}},
    \code{\link{plot.default}} or \code{\link{pairs}}.}
}
\details{
  This is intended for data frames with \emph{numeric} columns. For more
  than two columns it first calls \code{\link{data.matrix}} to convert
  the data frame to a numeric matrix and then calls \code{\link{pairs}}
  to produce a scatterplot matrix).  This can fail and may well be
  inappropriate: for example numerical conversion of dates will lose
  their special meaning and a warning will be given.

  For a two-column data frame it plots the second column against the
  first by the most appropriate method for the first column.

  For a single numeric column it uses \code{\link{stripchart}}, and for
  other single-column data frames tries to find a plot method for the
  single column.
}
\seealso{
  \code{\link{data.frame}}
}
\examples{
plot(OrchardSprays[1], method = "jitter")
plot(OrchardSprays[c(4,1)])
plot(OrchardSprays)

plot(iris)
plot(iris[5:4])
plot(women)
}
\keyword{hplot}
\keyword{methods}
