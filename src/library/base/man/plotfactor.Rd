\name{plot.factor}
\title{Plotting factor variables}
\usage{
plot.factor(x, y, legend.text = levels(y), \dots)
}
\alias{plot.factor}
\arguments{
    \item{x,y}{numeric or factor.  \code{y} may be missing.}
    \item{legend.text}{a vector of text used to construct a legend for
	the plot. Only used if \code{y} is present and a factor.}
    \item{\dots}{Further arguments to \code{\link{plot}}, see also
   	\code{\link{par}}.}
}
\description{
  This functions implements a ``scatterplot'' method for
  \code{\link{factor}} arguments of the \emph{generic}
  \code{\link{plot}} function.
  Actually, \code{\link{boxplot}} or \code{\link{barplot}} are used when
  appropriate.
}
\seealso{
    \code{\link{plot.default}}, \code{\link{plot.formula}},
    \code{\link{barplot}}, \code{\link{boxplot}}.
}
\examples{
data(PlantGrowth)
plot(PlantGrowth)                                # -> plot.data.frame
plot(weight ~ group, data = PlantGrowth)         # numeric vector ~ factor
plot(cut(weight, 2) ~ group, data = PlantGrowth) # factor ~ factor
}
\keyword{hplot}
