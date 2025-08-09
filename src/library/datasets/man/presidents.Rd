% File src/library/datasets/man/presidents.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{presidents}
\docType{data}
\alias{presidents}
\title{Quarterly Approval Ratings of US Presidents}
\description{
  The (approximately) quarterly approval rating for the President of the
  United States from the first quarter of 1945 to the last quarter of
  1974.
}
\usage{presidents}
\format{
  A time series of 120 values.
}
\source{
  The Gallup Organisation.
}
\details{
  The data are actually a fudged version of the approval ratings.  See
  McNeil's book for details.
}
\references{
  \bibshow{R:McNeil:1977}
}
\examples{
require(stats); require(graphics)
plot(presidents, las = 1, ylab = "Approval rating (\%)",
     main = "presidents data")
}
\keyword{datasets}
