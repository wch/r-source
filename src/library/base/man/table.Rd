\name{table}
\title{Cross Tabulation}
\usage{
table(\dots, exclude = c(NA, NaN), dnn, deparse.level = 1)
}
\alias{table}
\arguments{
  \item{\dots}{objects which can be interpreted as factors (including
    character strings), or a list (or data frame) whose components can
    be so interpreted}
  \item{exclude}{values to use in the exclude argument of \code{factor}
    when interpreting non-factor objects}
  \item{dnn}{the names to be given to the dimensions in the result (`the
  dimname names'}
  \item{deparse.level}{controls how the default \code{dnn} is
      constructed. See details.}
  }
\description{
  \code{table} uses the cross-classifying factors to build a contingency
  table of the counts at each combination of factor levels.
}
\details{
    If the argument \code{dnn} is not supplied, the internal function
    \code{list.names} is called to compute the `dimname names'. If the
    arguments in \code{\dots} are named, those names are used. For the
    remaining arguments, \code{deparse.level = 0} gives an empty name,
    \code{deparse.level = 1} uses the supplied argument if it is a
    symbol, and \code{deparse.level = 2} will deparse the argument.
}
\examples{
## Simple frequency distribution
table(rpois(100,5))
data(warpbreaks)
attach(warpbreaks)
## Check the design:
table(wool, tension)
data(state)
table(state.division, state.region)

data(airquality)
attach(airquality)
# simple two-way contingency table
table(cut(Temp, quantile(Temp)), Month)

a <- letters[1:3]
table(a, sample(a)) # dnn is  c("a", "")
table(a, sample(a), deparse.level = 0) # dnn is  c("", "")
table(a, sample(a), deparse.level = 2) # dnn is  c("a", "sample(a)")
}
\keyword{category}


