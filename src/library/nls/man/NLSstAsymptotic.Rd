% $Id: NLSstAsymptotic.Rd,v 1.3.14.1 2001/01/08 20:35:33 bates Exp $
\name{NLSstAsymptotic}
\title{Fit the Asymptotic Regression Model}
\alias{NLSstAsymptotic}
\alias{NLSstAsymptotic.sortedXyData}
\usage{
NLSstAsymptotic(xy)
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
\keyword{data}
\keyword{manip}
