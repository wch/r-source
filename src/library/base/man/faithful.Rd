\name{faithful}
\title{Old Faithful Geyser Data}
\usage{data(faithful)}
\alias{faithful}
\format{A data frame with 272 observations on 2 variables.
    \tabular{rlll}{
	[,1]  \tab eruptions  \tab numeric  \tab Eruption time in mins \cr
	[,2]  \tab waiting    \tab numeric  \tab Waiting time to next
	eruption \cr
    }
}
\source{W. Hardle.}
\description{
    The `faithful' data frame has 272 rows and 2 columns; the waiting
    time between  eruptions  and the  duration  of the eruption for
    the Old Faithful geyser in Yellowstone National Park, Wyoming, USA.
}
\references{
    Hardle, W. (1991).  Smoothing Techniques with Implementation in S.
    New York: Springer
    
    Azzalini, A. and Bowman, A.W. (1990) A look at some data on the Old
    Faithful geyser.  Applied Statistics 39, 357-365.
}
\examples{
data(faithful)
plot(faithful, main = "faithful data: Eruptions of Old Faithful",
     xlab = "Eruption time (min)",
     ylab = "Waiting time to next eruption (min)")
lines(lowess(faithful$eruptions, faithful$waiting, f = 2/3, iter = 3),
      col = "red")
}
\keyword{datasets}
