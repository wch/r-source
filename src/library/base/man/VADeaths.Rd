\name{VADeaths}
\title{Death Rate Data}
\usage{data(VADeaths)}
\alias{VADeaths}
\format{A matrix with 5 rows and 5 columns.}
\source{
    L. Moyneau, S. K. Gilliam, and L. C. Florant (1947).
    ``Differences in Virginia Death Rates by Color, Sex, Age,
    and Rural or Urban Residence'',
    American Sociological Review, \bold{12}, 525-535.
}
\description{
    Death rates per 100 in Virginia in 1940.  The death rates are
    cross-classified by age group (rows) and population group
    (columns).  The age groups are: 50-54, 55-59, 60-64, 65-69, 70-74
    and the population groups are Rural/Male, Rural/Female, Urban/Male
    and Urban/Female.

    This provides a rather nice 3-way analysis of variance example.
}
\references{
    McNeil, D. R. (1977).  Interactive Data Analysis.  Wiley.}
\examples{
data(VADeaths)
n <- length(dr <- c(VADeaths))
nam <- names(VADeaths)
d.VAD <- data.frame(
 Drate = dr,
 age = rep(ordered(rownames(VADeaths)),length=n),
 gender= gl(2,5,n, labels= c("M", "F")),
 site =  gl(2,10,  labels= c("rural", "urban")))

summary(aov.VAD <- aov(Drate ~ .^2, data = d.VAD))
plot(aov.VAD)
}
\keyword{datasets}
