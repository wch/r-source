% File src/library/graphics/man/fourfoldplot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{fourfoldplot}
\alias{fourfoldplot}
\title{Fourfold Plots}
\description{
  Creates a fourfold display of a 2 by 2 by \eqn{k} contingency table on
  the current graphics device, allowing for the visual inspection of the
  association between two dichotomous variables in one or several
  populations (strata).
}
\usage{
fourfoldplot(x, color = c("#99CCFF", "#6699CC"),
             conf.level = 0.95,
             std = c("margins", "ind.max", "all.max"),
             margin = c(1, 2), space = 0.2, main = NULL,
             mfrow = NULL, mfcol = NULL)
}
\arguments{
  \item{x}{a 2 by 2 by \eqn{k} contingency table in array form, or as a
    2 by 2 matrix if \eqn{k} is 1.}
  \item{color}{a vector of length 2 specifying the colors to use for the
    smaller and larger diagonals of each 2 by 2 table.}
  \item{conf.level}{confidence level used for the confidence rings on
    the odds ratios.  Must be a single nonnegative number less than 1;
    if set to 0, confidence rings are suppressed.}
  \item{std}{a character string specifying how to standardize the table.
    Must match one of \code{"margins"}, \code{"ind.max"}, or
    \code{"all.max"}, and can be abbreviated to the initial letter.
    If set to \code{"margins"}, each 2 by 2 table is standardized to
    equate the margins specified by \code{margin} while preserving the
    odds ratio.  If \code{"ind.max"} or \code{"all.max"}, the tables are
    either individually or simultaneously standardized to a maximal cell
    frequency of 1.}
  \item{margin}{a numeric vector with the margins to equate.  Must be
    one of \code{1}, \code{2}, or \code{c(1, 2)} (the default), which
    corresponds to standardizing the row, column, or both margins in
    each 2 by 2 table.
    Only used if \code{std} equals \code{"margins"}.}
  \item{space}{the amount of space (as a fraction of the maximal radius
    of the quarter circles) used for the row and column labels.}
  \item{main}{character string for the fourfold title.}
  \item{mfrow}{a numeric vector of the form \code{c(nr, nc)}, indicating
    that the displays for the 2 by 2 tables should be arranged in an
    \code{nr} by \code{nc} layout, filled by rows.}
  \item{mfcol}{a numeric vector of the form \code{c(nr, nc)}, indicating
    that the displays for the 2 by 2 tables should be arranged in an
    \code{nr} by \code{nc} layout, filled by columns.}
}
\details{
  The fourfold display is designed for the display of 2 by 2 by \eqn{k}
  tables.

  Following suitable standardization, the cell frequencies
  \eqn{f_{ij}}{f[i,j]} of each 2 by 2 table are shown as a quarter
  circle whose radius is proportional to
  \eqn{\sqrt{f_{ij}}}{sqrt(f[i,j])} so that its area is proportional to
  the cell frequency.  An association (odds ratio different from 1)
  between the binary row and column variables is indicated by the
  tendency of diagonally opposite cells in one direction to differ in
  size from those in the other direction; color is used to show this
  direction.  Confidence rings for the odds ratio allow a visual test of
  the null of no association; the rings for adjacent quadrants overlap
  if and only if the observed counts are consistent with the null hypothesis.

  Typically, the number \eqn{k} corresponds to the number of levels of a
  stratifying variable, and it is of interest to see whether the
  association is homogeneous across strata.  The fourfold display
  visualizes the pattern of association.  Note that the confidence rings
  for the individual odds ratios are not adjusted for multiple testing.
}
\references{
  Friendly, M. (1994).
  A fourfold display for 2 by 2 by \eqn{k} tables.
  Technical Report 217, York University, Psychology Department.
  \url{http://www.math.yorku.ca/SCS/Papers/4fold/4fold.ps.gz}
}
\seealso{
  \code{\link{mosaicplot}}
}
\examples{
## Use the Berkeley admission data as in Friendly (1995).
x <- aperm(UCBAdmissions, c(2, 1, 3))
dimnames(x)[[2]] <- c("Yes", "No")
names(dimnames(x)) <- c("Sex", "Admit?", "Department")
stats::ftable(x)

## Fourfold display of data aggregated over departments, with
## frequencies standardized to equate the margins for admission
## and sex.
## Figure 1 in Friendly (1994).
fourfoldplot(margin.table(x, c(1, 2)))

## Fourfold display of x, with frequencies in each table
## standardized to equate the margins for admission and sex.
## Figure 2 in Friendly (1994).
fourfoldplot(x)

## Fourfold display of x, with frequencies in each table
## standardized to equate the margins for admission. but not
## for sex.
## Figure 3 in Friendly (1994).
fourfoldplot(x, margin = 2)
}
\keyword{hplot}
