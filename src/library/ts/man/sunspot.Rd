\name{sunspot}
\alias{sunspot}
\alias{sunspot.month}
\alias{sunspot.year}
\title{
  Yearly Sunspot Data, 1700-1988.
  Monthly Sunspot Data, 1749-1997.
}
\usage{
data(sunspot)
}
\format{
    The univariate time series \code{sunspot.year} and
    \code{sunspot.month} contain 289 and 2988 observations, respectively.
    The objects are of class \code{"ts"}.
}
\description{
    Monthly and yearly number of sunspots. 
}
\seealso{
    \code{sunspot.month} is a longer version of \code{\link{sunspots}}
    in base \R, that runs until 1988.
}
\source{
    Monthly data: SUNSPOT INDEX DATA CENTER,
    WORLD DATA CENTER-C1 FOR SUNSPOT INDEX
    ROYAL OBSERVATORY OF BELGIUM, Av. Circulaire, 3 - B-1180 BRUSSELS
    \url{http://www.oma.be/KSB-ORB/SIDC/sidc\_txt.html}

    Yearly data: H. Tong (1996)
    \emph{Non-Linear Time Series}. Clarendon Press, Oxford, p. 471. 
}
\keyword{datasets}


