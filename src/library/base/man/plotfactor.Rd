\name{plot.factor}
\title{Plotting factor variables}
\usage{
plot.factor(x, y, \dots)
}
\alias{plot.factor}
\arguments{
 \item{x,y}{numeric or factor.  \code{y} may be missing.}
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
  \code{\link{plot.default}}, \code{\link{plot.formula}}.
}
\examples{
data(plants)
plot(plants)#-> plot.data.frame
plot(weight ~ group, data=plants)#-> plot.formula and plot.factor
}
\keyword{hplot}



