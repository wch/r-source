% File src/library/stats/man/ave.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{ave}
\title{Group Averages Over Level Combinations of Factors}
\usage{
ave(x, \dots, FUN = mean)
}
\alias{ave}
\arguments{
  \item{x}{A numeric.}
  \item{\dots}{Grouping variables, typically factors, all of the same
    \code{length} as \code{x}.}
  \item{FUN}{Function to apply for each factor level combination.}
}
\description{
  Subsets of \code{x[]} are averaged, where each subset consist of those
  observations with the same factor levels.
}
\value{
  A numeric vector, say \code{y} of length \code{length(x)}.
  If \code{\dots} is \code{g1, g2}, e.g.,
  \code{y[i]} is equal to \code{FUN(x[j]}, for all \code{j} with
  \code{g1[j] == g1[i]} and \code{g2[j] == g2[i])}.
}
\seealso{\code{\link{mean}}, \code{\link{median}}.}
\examples{
require(graphics)

ave(1:3)  # no grouping -> grand mean

attach(warpbreaks)
ave(breaks, wool)
ave(breaks, tension)
ave(breaks, tension, FUN = function(x) mean(x, trim = 0.1))
plot(breaks, main =
     "ave( Warpbreaks )  for   wool  x  tension  combinations")
lines(ave(breaks, wool, tension              ), type = "s", col = "blue")
lines(ave(breaks, wool, tension, FUN = median), type = "s", col = "green")
legend(40, 70, c("mean", "median"), lty = 1,
      col = c("blue","green"), bg = "gray90")
detach()
}
\keyword{univar}

