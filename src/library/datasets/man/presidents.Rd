\name{presidents}
\docType{data}
\alias{presidents}
\title{Quarterly Approval Ratings of US Presidents}
\description{
  The (approximately) quarterly approval rating for the President of the
  United states from the first quarter of 1945 to the last quarter of
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
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
plot(presidents, las = 1, ylab = "Approval rating (\%)",
     main = "presidents data")
}
\keyword{datasets}
