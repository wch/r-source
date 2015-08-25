% File src/library/base/man/transform.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{transform}
\alias{transform}
\alias{transform.default}
\alias{transform.data.frame}
\title{Transform an Object, for Example a Data Frame}
\usage{
transform(`_data`, \dots)
}
\arguments{
    \item{_data}{The object to be transformed}
    \item{\dots}{Further arguments of the form \code{tag=value}}
}
\description{
  \code{transform} is a generic function, which---at least
  currently---only does anything useful with
  data frames.  \code{transform.default} converts its first argument to
  a data frame if possible and calls \code{transform.data.frame}.
}
\details{
  The \code{\dots} arguments to \code{transform.data.frame} are tagged
  vector expressions, which are evaluated in the data frame
  \code{_data}.  The tags are matched against \code{names(_data)}, and for
  those that match, the value replace the corresponding variable in
  \code{_data}, and the others are appended to \code{_data}.
}
\value{
  The modified value of \code{_data}.
}
\section{Warning}{
  This is a convenience function intended for use interactively.  For
  programming it is better to use the standard subsetting arithmetic functions,
  and in particular the non-standard evaluation of
  argument \code{transform} can have unanticipated consequences.
  
}
\author{Peter Dalgaard}
\note{
  If some of the values are not vectors of the appropriate length,
  you deserve whatever you get!
}
\seealso{ \code{\link{within}} for a more flexible approach,
    \code{\link{subset}},
    \code{\link{list}},
    \code{\link{data.frame}}
}
\examples{
transform(airquality, Ozone = -Ozone)
transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8)

attach(airquality)
transform(Ozone, logOzone = log(Ozone)) # marginally interesting ...
detach(airquality)
}
\keyword{manip}
