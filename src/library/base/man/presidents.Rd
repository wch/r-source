\name{presidents}
\alias{presidents}
\title{Approval Rating of US Presidents}
\usage{data(presidents)}
\format{
  A time series of 120 values.
}
\source{
  The Gallup Organisation.
}
\description{
  The (approximately) quarterly approval rating for the President of the
  United states from the first quarter of 1945 to the last quarter of
  1974.
}
\details{
  The data are actually a fudged version of the approval ratings.  See
  McNeil's book for details.

\references{
  McNeil, D. R. (1977) \emph{Interactive Data Analysis.}  New York:
  Wiley.
}
\examples{
data(presidents)
plot(presidents, las = 1, ylab = "Approval rating (\%)",
     main = "presidents data")
}
\keyword{datasets}
