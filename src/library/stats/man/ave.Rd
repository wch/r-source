% File src/library/stats/man/ave.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{ave}
\alias{ave}
\title{Group Averages Over Level Combinations of Factors}
\description{
  A function is applied to subsets of a vector \code{x}, where each
  subset consists of those observations with the same factor levels.
  The \code{\link{mean}} function is applied by default,
  replacing the values in each group by the group \emph{ave}rage.
}
\usage{
ave(x, \dots, FUN = mean)
}
\arguments{
  \item{x}{a vector, typically numeric.}
  \item{\dots}{grouping variables, typically factors, all of the same
    length as \code{x}. Groups are defined by the
    \code{\link{interaction}} of these variables.}
  \item{FUN}{a function to apply for each factor level combination.
    (NOTE: If given, this argument must be named.)}
}
\value{
  A vector, say \code{y}, of the same length as \code{x}.
  If grouping variables are missing, all elements of \code{y} are equal
  to \code{FUN(x)}. Otherwise, if, e.g., \code{\dots} is \code{g1, g2},
  \code{y[i]} is equal to \code{FUN(x[sub])} with \code{sub} comprising all
  \code{j} for which \code{g1[j] == g1[i]} and \code{g2[j] == g2[i]}.
}
\seealso{
  \code{\link{mean}}, \code{\link{split}}, \code{\link{tapply}}.
}
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

# Running index per group
(g <- sample(c("u","s","e","R"), 24, replace = TRUE))
ave(seq_along(g), g, FUN = seq_along)
}
\keyword{univar}
\keyword{category}
