\name{islands}
\alias{islands}
\title{Areas of the World's Major Landmasses}
\usage{data(islands)}
\format{A named vector of length 48.}
\source{The World Almanac and Book of Facts, 1975, page 406.}
\description{
  The areas in thousands of square miles of the landmasses which exceed
  10,000 square miles.
}
\references{
  McNeil, D. R. (1977) \emph{Interactive Data Analysis.}  Wiley.
}
\examples{
data(islands)
dotplot(log(islands, 10),
   main = "islands data: log10(area) (log10(sq. miles))")
dotplot(log(islands[order(islands)], 10),
   main = "islands data: log10(area) (log10(sq. miles))")
}
\keyword{datasets}
