% $Id: NLSstRtAsymptote.Rd,v 1.4 2001/03/24 06:57:57 ripley Exp $
\name{NLSstRtAsymptote}
\title{Horizontal Asymptote on the Right Side}
\alias{NLSstRtAsymptote}
\alias{NLSstRtAsymptote.sortedXyData}
\usage{
NLSstRtAsymptote(xy)
}
\arguments{
 \item{xy}{a \code{sortedXyData} object}
}
\description{
  Provide an initial guess at the horizontal asymptote on
  the right side (i.e. large values of \code{x}) of the graph of \code{y}
  versus \code{x} from the \code{xy} object.  Primarily used within
  \code{initial} functions for self-starting nonlinear regression
  models.
}
\value{
  A single numeric value estimating the horizontal asymptote for large \code{x}.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{sortedXyData}}, \code{\link{NLSstClosestX}},
  \code{\link{NLSstRtAsymptote}}, \code{\link{selfStart}}}
\examples{
data( DNase )
DNase.2 <- DNase[ DNase$Run == "2", ]
DN.srt <- sortedXyData( expression(log(conc)), expression(density), DNase.2 )
NLSstRtAsymptote( DN.srt )
}
\keyword{manip}
