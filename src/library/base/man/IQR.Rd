\name{IQR}
\title{The Interquartile Range}
\usage{
IQR(x)
}
\alias{IQR}
\description{
This function computes interquartile range of the data
values in its arguments.
Note that this function compute the quartiles using the function
\code{quantile} rather than following Tukey's recommendations.
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

