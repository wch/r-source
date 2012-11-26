% File src/library/graphics/man/boxplot.matrix.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{boxplot.matrix}
\alias{boxplot.matrix}
\title{Draw a Boxplot for each Column (Row) of a Matrix}
\description{
  Interpreting the columns (or rows) of a matrix as different groups, draw a
  boxplot for each.
}
\usage{
\method{boxplot}{matrix}(x, use.cols = TRUE, \dots)
}
\arguments{
  \item{x}{a numeric matrix.}
  \item{use.cols}{logical indicating if columns (by default) or rows
    (\code{use.cols = FALSE}) should be plotted.}
  \item{\dots}{Further arguments to \code{\link{boxplot}}.}
}
\value{
  A list as for \code{\link{boxplot}}.
}
\author{Martin Maechler, 1995, for S+, then \R package \CRANpkg{sfsmisc}.}
\seealso{\code{\link{boxplot.default}} which already works nowadays with
  data.frames; \code{\link{boxplot.formula}}, \code{\link{plot.factor}} which
  work with (the more general concept) of a grouping factor.
}
\examples{
## Very similar to the example in ?boxplot
mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100),
             T5 = rt(100, df = 5), Gam2 = rgamma(100, shape = 2))
boxplot(mat, main = "boxplot.matrix(...., main = ...)",
        notch = TRUE, col = 1:4)
}
\keyword{hplot}
