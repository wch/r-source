\name{mad}
\title{Median Absolute Deviation}
\usage{
mad(x, center, constant=1.4826, na.rm=FALSE)
}
\alias{mad}
\description{
\code{mad} provides a scale estimate based on the
median absolute deviation.
The actual value calculated is
\code{constant * (median(abs(x - center)))}
with the default value of
\code{center} being \code{median(x)}.

If \code{na.rm} is \code{TRUE} then \code{NA}
values are stripped from \code{x} before computation takes place.
If this is not done then an \code{NA} value in
\code{x} will cause \code{mad} to return \code{NA}.
}
\seealso{
\code{\link{median}}, \code{\link{var}}.
}
\keyword{univar}
\keyword{robust}
