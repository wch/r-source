% $Id: NLSstLfAsymptote.Rd,v 1.1 2003/12/11 07:16:06 ripley Exp $
\name{NLSstLfAsymptote}
\title{Horizontal Asymptote on the Left Side}
\alias{NLSstLfAsymptote}
\alias{NLSstLfAsymptote.sortedXyData}
\description{
  Provide an initial guess at the horizontal asymptote on the left side
  (i.e., small values of \code{x}) of the graph of \code{y} versus
  \code{x} from the \code{xy} object.  Primarily used within
  \code{initial} functions for self-starting nonlinear regression
  models.
}
\usage{
NLSstLfAsymptote(xy)
}
\arguments{
 \item{xy}{a \code{sortedXyData} object}
}
\value{
  A single numeric value estimating the horizontal asymptote for small
  \code{x}. 
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{
  \code{\link{sortedXyData}},
  \code{\link{NLSstClosestX}},
  \code{\link{NLSstRtAsymptote}},
  \code{\link{selfStart}}
}
\examples{
DNase.2 <- DNase[ DNase$Run == "2", ]
DN.srt <- sortedXyData( expression(log(conc)), expression(density), DNase.2 )
NLSstLfAsymptote( DN.srt )
}
\keyword{manip}
