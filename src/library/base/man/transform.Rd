\name{transform}
\title{Transform an object, e.g. a dataframe}
\usage{
transform(x, ...)
}
\usage{
transform.data.frame(x, ...)
}
\usage{
transform.default(x, ...)
}
\alias{transform}
\alias{transform.data.frame}
\alias{transform.default}
%- Also NEED an `\alias' for EACH other function documented here.
\arguments{
 \item{x}{The object to be transformed}
 \item{\dots}{Further arguments of the form \code{tag=value}}
}
\description{
 \code{transform} is a generic function, which --  at least currently
-- only does anything useful with dataframes. \code{transform.default}
converts its first argument to a dataframe if possible and calls
\code{transform.data.frame}. 

The \code{...} arguments to \code{transform.data.frame} are tagged
vector expressions, which are evaluated in the dataframe \code{x}. The
tags are matched against \code{names(x)}, and for those that match,
the value replace the corresponding variable in \code{x}, and the
others are appended to \code{x}} 

\value{
  The modified value of \code{x}
}
\author{Peter Dalgaard}
\note{If some of the values are not vectors of the appropriate length,
you deserve whatever you get!}

\seealso{\code{\link{subset}}, \code{\link{list}},
\code{\link{data.frame}}} 

\examples{
data(airquality)
transform(airquality,Ozone=-Ozone)
transform(airquality,new=-Ozone,Temp=(Temp-32)/1.8)

attach(airquality)
transform(Ozone,logOzone=log(Ozone)) # marginally interesting...
detach(airquality)
}
\keyword{manip}%-- one or more ...
