\name{transform}
\alias{transform}
\alias{transform.default}
\alias{transform.data.frame}
\title{Transform an Object, for Example a Data Frame}
\usage{
transform(x, \dots)
%transform.default(x, \dots)
%transform.data.frame(x, \dots)
}
\arguments{
    \item{x}{The object to be transformed}
    \item{\dots}{Further arguments of the form \code{tag=value}}
}
\description{
  \code{transform} is a generic function, which---at least
  currently---only does anything useful with
  data frames. \code{transform.default} converts its first argument to
  a data frame if possible and calls \code{transform.data.frame}. 
}
\details{
  The \code{\dots} arguments to \code{transform.data.frame} are tagged
  vector expressions, which are evaluated in the data frame
  \code{x}.  The tags are matched against \code{names(x)}, and for
  those that match, the value replace the corresponding variable in
  \code{x}, and the others are appended to \code{x}.
} 
\value{
  The modified value of \code{x}.
}
\author{Peter Dalgaard}
\note{If some of the values are not vectors of the appropriate length,
  you deserve whatever you get!}
\seealso{
    \code{\link{subset}},
    \code{\link{list}},
    \code{\link{data.frame}}
}
\examples{
data(airquality)
transform(airquality, Ozone = -Ozone)
transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8)

attach(airquality)
transform(Ozone, logOzone = log(Ozone)) # marginally interesting ...
detach(airquality)
}
\keyword{manip}
