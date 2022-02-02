% File src/library/graphics/man/plotfactor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{plot.factor}
\title{Plotting Factor Variables}
\usage{
\method{plot}{factor}(x, y, legend.text = NULL, \dots)
}
\alias{plot.factor}
\arguments{
    \item{x, y}{numeric or factor. \code{y} may be missing.}
    \item{legend.text}{character vector for annotation of y axis in the
      case of a factor \code{y}: defaults to \code{levels(y)}.  This
      sets the \code{yaxlabels} argument of \code{\link{spineplot}}.}
    \item{\dots}{Further arguments to \code{\link{barplot}},
      \code{\link{boxplot}}, \code{\link{spineplot}} or
      \code{\link{plot}} as appropriate.  All of these accept
      \link{graphical parameters} (see \code{\link{par}}) and
      annotation arguments passed
      to \code{\link{title}} and \code{axes = FALSE}.  None accept \code{type}.
    }
}
\description{
  This functions implements a scatterplot method for
  \code{\link{factor}} arguments of the \emph{generic}
  \code{\link{plot}} function.

  If \code{y} is missing \code{\link{barplot}} is produced.
  For numeric \code{y} a \code{\link{boxplot}} is used, and for a factor
  \code{y} a \code{\link{spineplot}} is shown.  For any other type of
  \code{y} the next \code{plot} method is called, normally
  \code{\link{plot.default}}.
}
\seealso{
    \code{\link{plot.default}}, \code{\link{plot.formula}},
    \code{\link{barplot}}, \code{\link{boxplot}}, \code{\link{spineplot}}.
}
\examples{
require(grDevices)
% this does not call plot.factor!
% plot(PlantGrowth)                                # -> plot.data.frame
plot(weight ~ group, data = PlantGrowth)           # numeric vector ~ factor
plot(cut(weight, 2) ~ group, data = PlantGrowth)   # factor ~ factor
## passing "..." to spineplot() eventually:
plot(cut(weight, 3) ~ group, data = PlantGrowth,
     col = hcl(c(0, 120, 240), 50, 70))

plot(PlantGrowth$group, axes = FALSE, main = "no axes")  # extremely silly
}
\keyword{hplot}
