\name{discoveries}
\title{Numbers of Important Discoveries}
\usage{data(discoveries)}
\alias{discoveries}
\format{A time series of 100 values.
}
\source{The World Almanac and Book of Facts, 1975 Edition, pages 315-318.
}
\description{The numbers of "great" inventions and scientific
  discoveries in each year from 1860 to 1959.
}
\references{McNeil, D. R. (1977).  Interactive Data Analysis.  Wiley.
}
\examples{
data(discoveries)
plot(discoveries, ylab = "Number of important discoveries",
     las = 1)
title(main = "discoveries data set")
}
\keyword{datasets}


