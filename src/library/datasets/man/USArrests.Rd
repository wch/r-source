% File src/library/datasets/man/USArrests.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{USArrests}
\title{Violent Crime Rates by US State}
\docType{data}
\alias{USArrests}
\description{
  This data set contains statistics, in arrests per 100,000 residents
  for assault, murder, and rape in each of the 50 US states in 1973.
  Also given is the percent of the population living in urban areas.
}
\usage{USArrests}
\format{
  A data frame with 50 observations on 4 variables.

  \tabular{rlll}{
    [,1]  \tab Murder    \tab numeric \tab Murder arrests (per 100,000)\cr
    [,2]  \tab Assault   \tab numeric \tab Assault arrests (per 100,000)\cr
    [,3]  \tab UrbanPop  \tab numeric \tab Percent urban population\cr
    [,4]  \tab Rape      \tab numeric \tab Rape arrests (per 100,000)
  }
}
\source{
  World Almanac and Book of facts 1975.  (Crime rates).

  Statistical Abstracts of the United States 1975, p.20, (Urban rates),
  possibly available as
  \url{https://books.google.ch/books?id=zl9qAAAAMAAJ&pg=PA20}.
}
\note{
  \code{USArrests} contains the data as in McNeil's monograph.  For the
  \code{UrbanPop} percentages, a review of the table (No. 21) in the
  Statistical Abstracts 1975 reveals a transcription error for Maryland
  (and that McNeil used the same \dQuote{round to even} rule that \R's
  \code{\link{round}()} uses), as found by Daniel S Coven (Arizona).

  See the example below on how to correct the error and improve accuracy
  for the \sQuote{<n>.5} percentages.
}
\seealso{The \code{\link{state}} data sets.}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
summary(USArrests)

require(graphics)
pairs(USArrests, panel = panel.smooth, main = "USArrests data")

## Difference between 'USArrests' and its correction
USArrests["Maryland", "UrbanPop"] # 67 -- the transcription error
UA.C <- USArrests
UA.C["Maryland", "UrbanPop"] <- 76.6

## also +/- 0.5 to restore the original  <n>.5  percentages
s5u <- c("Colorado", "Florida", "Mississippi", "Wyoming")
s5d <- c("Nebraska", "Pennsylvania")
UA.C[s5u, "UrbanPop"] <- UA.C[s5u, "UrbanPop"] + 0.5
UA.C[s5d, "UrbanPop"] <- UA.C[s5d, "UrbanPop"] - 0.5

## ==> UA.C  is now a *C*orrected version of  USArrests
}
\keyword{datasets}
