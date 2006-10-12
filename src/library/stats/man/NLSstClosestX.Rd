% $Id: NLSstClosestX.Rd,v 1.1 2003/12/11 07:16:06 ripley Exp $
\name{NLSstClosestX}
\title{Inverse Interpolation}
\alias{NLSstClosestX}
\alias{NLSstClosestX.sortedXyData}
\usage{
NLSstClosestX(xy, yval)
}
\arguments{
 \item{xy}{a \code{sortedXyData} object}
 \item{yval}{a numeric value on the \code{y} scale}
}
\description{
  Use inverse linear interpolation to approximate the \code{x} value at
  which the function represented by \code{xy} is equal to \code{yval}.
}
\value{
  A single numeric value on the \code{x} scale.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{sortedXyData}}, \code{\link{NLSstLfAsymptote}},
  \code{\link{NLSstRtAsymptote}}, \code{\link{selfStart}}}
\examples{
DNase.2 <- DNase[ DNase$Run == "2", ]
DN.srt <- sortedXyData(expression(log(conc)), expression(density), DNase.2)
NLSstClosestX(DN.srt, 1.0)
}
\keyword{manip}
