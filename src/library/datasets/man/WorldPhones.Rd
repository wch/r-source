\name{WorldPhones}
\docType{data}
\alias{WorldPhones}
\title{The World's Telephones}
\description{
  The number of telephones in various regions of the world (in
  thousands).
}
\usage{phones}
\format{
  A matrix with 7 rows and 8 columns.  The columns of the matrix give
  the figures for a given region, and the rows the figures for a year.

  The regions are: North America, Europe, Asia, South America, Oceania,
  Africa, Central America.
  
  The years are: 1951, 1956, 1957, 1958, 1959, 1960, 1961.
}
\source{
  AT&T (1961) \emph{The World's Telephones}.
}
\section{Warning}{
  Prior to \R 2.0.0 this dataset was called \code{phones} and conflicted
  with a dataset in package \pkg{MASS}.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
matplot(rownames(WorldPhones), WorldPhones, type = "b", log = "y",
        xlab = "Year", ylab = "Number of telephones (1000's)")
legend(1951.5, 80000, colnames(WorldPhones), col = 1:6, lty = 1:5, 
       pch = rep(21, 7))
title(main = "World phones data: log scale for response")
}
\keyword{datasets}
