% $Id: NLSstAsymptotic.Rd,v 1.1 1999/11/15 23:20:07 bates Exp $
\name{NLSstAsymptotic}
\title{Fit the asymptotic regression model}
\alias{NLSstAsymptotic}
\alias{NLSstAsymptotic.sortedXyData}
\usage{
NLSstAsymptotic(xy, yval)
}
\arguments{
 \item{xy}{a \code{sortedXyData} object}
}
\description{
  Fits the asymptotic regression model, in the form \code{b0 +
      b1*exp(-exp(lrc) * x)} to the \code{xy} data.  
  This can be used as a building block in determining starting estimates
  for more complicated models.
}
\value{
  A numeric value of length 3 with components labelled \code{b0},
  \code{b1}, and \code{lrc}
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{SSasymp}}}
\examples{
library( nls )
data( Loblolly )
Lob.329 <- Loblolly[ Loblolly$Seed == "329", ]
NLSstAsymptotic(sortedXyData(expression(age), expression(height), Lob.329 ))
}
\keyword{data, manip}
