\name{plot.factor}
\title{Plotting Factor Variables}
\usage{
\method{plot}{factor}(x, y, legend.text = NULL, \dots)
}
\alias{plot.factor}
\arguments{
    \item{x, y}{numeric or factor.  \code{y} may be missing.}
    \item{legend.text}{character vector for annotation of y axis,
      defaults to \code{levels(y)}. Can be used instead of \code{yaxlabels}
      (for backward compatibility).}      
    \item{\dots}{Further arguments to \code{\link{plot}}, see also
        \code{\link{par}}.}
}
\description{
  This functions implements a \dQuote{scatterplot} method for
  \code{\link{factor}} arguments of the \emph{generic}
  \code{\link{plot}} function.
  Actually, \code{\link{boxplot}} or \code{\link{spineplot}} are used when
  appropriate.
}
\seealso{
    \code{\link{plot.default}}, \code{\link{plot.formula}},
    \code{\link{spineplot}}, \code{\link{boxplot}}.
}
\examples{
plot(PlantGrowth)                                # -> plot.data.frame
plot(weight ~ group, data = PlantGrowth)         # numeric vector ~ factor
plot(cut(weight, 2) ~ group, data = PlantGrowth) # factor ~ factor
## passing "..." to spineplot() eventually:
plot(cut(weight, 3) ~ group, data = PlantGrowth, col = hcl(c(0, 120, 240), 50, 70))

plot(PlantGrowth$group, axes=FALSE, main="no axes")# extremly silly
}
\keyword{hplot}
