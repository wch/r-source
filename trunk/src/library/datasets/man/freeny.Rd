% File src/library/datasets/man/freeny.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{freeny}
\docType{data}
\alias{freeny}
\alias{freeny.x}
\alias{freeny.y}
\title{Freeny's Revenue Data}
\description{
  Freeny's data on quarterly revenue and explanatory variables.
}
\usage{
freeny
freeny.x
freeny.y
}
\format{
  There are three \sQuote{freeny} data sets.

  \code{freeny.y} is a time series with 39 observations on quarterly
  revenue from (1962,2Q) to (1971,4Q).

  \code{freeny.x} is a matrix of explanatory variables.  The columns
  are \code{freeny.y} lagged 1 quarter, price index, income level, and
  market potential.

  Finally, \code{freeny} is a data frame with variables \code{y},
  \code{lag.quarterly.revenue}, \code{price.index}, \code{income.level},
  and \code{market.potential} obtained from the above two data objects.
}
\source{
  A. E. Freeny (1977)
  \emph{A Portable Linear Regression Package with Test Programs}.
  Bell Laboratories memorandum.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
require(stats); require(graphics)
summary(freeny)
pairs(freeny, main = "freeny data")
# gives warning: freeny$y has class "ts"

summary(fm1 <- lm(y ~ ., data = freeny))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)
}
\keyword{datasets}
