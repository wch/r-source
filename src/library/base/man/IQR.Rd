\name{IQR}
\title{The Interquartile Range}
\usage{
IQR(x, na.rm = FALSE)
}
\alias{IQR}
\description{
This function computes interquartile range of the \code{x}
values.
Note that this function computes the quartiles using the
\code{\link{quantile}} function rather than following Tukey's recommendations.
}
\references{
Tukey, J. W. (1977).
\emph{Exploratory Data Analysis}.
Reading: Addison-Wesley.
}
\seealso{
\code{\link{fivenum}}, \code{\link{range}}, \code{\link{quantile}}.
}
\examples{
data(rivers)
IQR(rivers)
}
\keyword{univar}
\keyword{robust}
\keyword{distribution}

