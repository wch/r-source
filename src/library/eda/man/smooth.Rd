\name{smooth}
\title{Median Smoothing}
\usage{
smooth(x)
}
\arguments{
\item{x}{a vector or time series}
}
\description{
This function applies the smoothing method \emph{3RSR}
to the data in \code{x}.
}
\value{
A vector or time series containing the smoothed values.
Note that there are other smoothing methods which provide
rather better results.
}
\references{
Tukey, J. W. (1977).
\emph{Exploratory Data Analysis},
Reading Massachusetts: Addison-Wesley.
}
\seealso{
\code{\link{lowess}}.
}
\examples{
library(eda)
data(presidents)
presidents[is.na(presidents)] <- 0
plot(presidents)
lines(smooth(presidents))
}
