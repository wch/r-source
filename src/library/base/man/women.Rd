\name{women}
\title{Average Heights and Weights for American Women}
\usage{data(women)}
\alias{women}
\format{A data frame with 15 observations on 2 variables.
    \tabular{rlll}{
	[,1]  \tab height  \tab numeric  \tab Height (in)\cr
	[,2]  \tab weight  \tab numeric  \tab Weight (lbs)
    }
}
\source{The World Almanac and Book of Facts, 1975.}
\description{
    This data set gives the average heights and weights for
    American women aged 30-39.  The data set appears to have been taken
    from the American Society of Actuaries \emph{Build and Blood Pressure
	Study} for some (unknown to us) earlier year.
    
    The World Almanac notes: ``The figures represent weights in ordinary
    indoor clothing and shoes, and heights with shoes.
}
\references{
    McNeil, D. R. (1977).  Interactive Data Analysis.  Wiley.
}
\examples{
data(women)
plot(women, xlab = "Height (in)", ylab = "Weight (lb)",
     main = "women data: American women aged 30-39")
}
\keyword{datasets}
