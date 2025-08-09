% File src/library/graphics/man/assocplot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{assocplot}
\alias{assocplot}
\encoding{UTF-8}
\title{Association Plots}
\description{
  Produce a Cohen-Friendly association plot indicating deviations from
  independence of rows and columns in a 2-dimensional contingency
  table.
}
\usage{
assocplot(x, col = c("black", "red"), space = 0.3,
          main = NULL, xlab = NULL, ylab = NULL)
}
\arguments{
  \item{x}{a two-dimensional contingency table in matrix form.}
  \item{col}{a character vector of length two giving the colors used for
    drawing positive and negative Pearson residuals, respectively.}
  \item{space}{the amount of space (as a fraction of the average
    rectangle width and height) left between each rectangle.}
  \item{main}{overall title for the plot.}
  \item{xlab}{a label for the x axis.  Defaults to the name (if any) of
    the row dimension in \code{x}.}
  \item{ylab}{a label for the y axis.  Defaults to the name (if any) of
    the column dimension in \code{x}.}
}
\details{
  For a two-way contingency table, the signed contribution to Pearson's
  \eqn{\chi^2}{chi^2} for cell \eqn{i, j} is \eqn{d_{ij} = (f_{ij} -
    e_{ij}) / \sqrt{e_{ij}}}{d_{ij} = (f_{ij} - e_{ij}) / sqrt(e_{ij})},
  where \eqn{f_{ij}} and \eqn{e_{ij}} are the observed and expected
  counts corresponding to the cell.  In the Cohen-Friendly association
  plot, each cell is represented by a rectangle that has (signed) height
  proportional to \eqn{d_{ij}} and width proportional to
  \eqn{\sqrt{e_{ij}}}{sqrt(e_{ij})}, so that the area of the box is
  proportional to the difference in observed and expected frequencies.
  The rectangles in each row are positioned relative to a baseline
  indicating independence (\eqn{d_{ij} = 0}).  If the observed frequency
  of a cell is greater than the expected one, the box rises above the
  baseline and is shaded in the color specified by the first element of
  \code{col}, which defaults to black; otherwise, the box falls below
  the baseline and is shaded in the color specified by the second
  element of \code{col}, which defaults to red.

  A more flexible and extensible implementation of association plots
  written in the grid graphics system is provided in the function
  \code{\link[vcd]{assoc}} in the contributed package \CRANpkg{vcd}
  \bibcitep{R:Meyer+Zeileis+Hornik:2006}.
}
\seealso{
  \code{\link{mosaicplot}}, \code{\link{chisq.test}}.
}
\references{
  \bibshow{*, R:Cohen:1980, R:Friendly:1992a}
}
\examples{
## Aggregate over sex:
x <- marginSums(HairEyeColor, c(1, 2))
x
assocplot(x, main = "Relation between hair and eye color")
}
\keyword{hplot}
