% File src/library/datasets/man/UKLungDeaths.Rd
% Part of the R package, https://www.R-project.org
% Copyright  1994-9 W. N. Venables and B. D. Ripley
% Distributed under GPL 2 or later

\name{UKLungDeaths}
\docType{data}
\alias{UKLungDeaths}
\alias{ldeaths}
\alias{fdeaths}
\alias{mdeaths}
\title{
Monthly Deaths from Lung Diseases in the UK
}
\usage{
ldeaths
fdeaths
mdeaths
}
\description{
    Three time series giving the monthly deaths from bronchitis,
    emphysema and asthma in the UK, 1974--1979,
    both sexes (\code{ldeaths}), males (\code{mdeaths}) and
    females (\code{fdeaths}).
}
\source{
P. J. Diggle (1990)
\emph{Time Series: A Biostatistical Introduction.}
Oxford, table A.3
}
\examples{
require(stats); require(graphics) # for time
plot(ldeaths)
plot(mdeaths, fdeaths)
## Better labels:
yr <- floor(tt <- time(mdeaths))
plot(mdeaths, fdeaths,
     xy.labels = paste(month.abb[12*(tt - yr)], yr-1900, sep = "'"))
}
\keyword{datasets}
