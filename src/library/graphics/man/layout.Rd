% File src/library/graphics/man/layout.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{layout}
\alias{layout}
\alias{layout.show}
\alias{lcm}
\title{Specifying Complex Plot Arrangements}
\author{Paul R. Murrell}
\description{
  \code{layout} divides the device up into as many rows and columns as
  there are in matrix \code{mat}, with the column-widths and the
  row-heights specified in the respective arguments.
}
\usage{
layout(mat, widths = rep.int(1, ncol(mat)),
       heights = rep.int(1, nrow(mat)), respect = FALSE)

layout.show(n = 1)
lcm(x)
}
\arguments{
  \item{mat}{a matrix object specifying the location of the next
    \eqn{N} figures on the output device.  Each value in the matrix must
    be \code{0} or a positive integer.  If \eqn{N} is the largest
    positive integer in the matrix, then the integers
    \eqn{\{1, \dots, N-1\}}{{1, \dots, N-1}} must also appear at least once in the matrix.}
  \item{widths}{a vector of values for the widths of columns on the
    device.  Relative widths are specified with numeric values.    Absolute
    widths (in centimetres) are specified with the \code{lcm()} function (see
    examples).}
  \item{heights}{a vector of values for the heights of rows on the device.
    Relative and absolute heights can be specified, see \code{widths} above.}
  \item{respect}{either a logical value or a matrix object.  If the
    latter, then it must have the same dimensions as \code{mat} and
    each value in the matrix must be either \code{0} or \code{1}.}
  \item{n}{number of figures to plot.}
  \item{x}{a dimension to be interpreted as a number of centimetres.}
}
\details{
    Figure \eqn{i} is allocated a region composed from a subset
    of these rows and columns, based on the rows and columns
    in which \eqn{i} occurs in \code{mat}.

    The \code{respect} argument controls whether a unit column-width is
    the same physical measurement on the device as a unit row-height.

    There is a limit (currently 200) for the numbers of rows and columns
    in the layout, and also for the total number of cells (10007).

    \code{layout.show(n)} plots (part of) the current layout, namely the
    outlines of the next \code{n} figures.

    \code{lcm} is a trivial function, to be used as \emph{the} interface
    for specifying absolute dimensions for the \code{widths} and
    \code{heights} arguments of \code{layout()}.
}
\value{
  \code{layout} returns the number of figures, \eqn{N}, see above.
}
\references{
  Murrell, P. R. (1999).
  Layouts: A mechanism for arranging plots on a page.
  \emph{Journal of Computational and Graphical Statistics},
  \bold{8}, 121--134.
  \doi{10.2307/1390924}.

  Chapter 5 of Paul Murrell's Ph.D. thesis.

  Murrell, P. (2005).
  \emph{R Graphics}.
  Chapman & Hall/CRC Press.
}
\seealso{
  \code{\link{par}} with arguments \code{mfrow}, \code{mfcol}, or
  \code{mfg}.
}
\section{Warnings}{
  These functions are totally incompatible with the other mechanisms for
  arranging plots on a device: \code{\link{par}(mfrow)},
  \code{par(mfcol)} and \code{\link{split.screen}}.
}
\examples{
def.par <- par(no.readonly = TRUE) # save default, for resetting...

## divide the device into two rows and two columns
## allocate figure 1 all of row 1
## allocate figure 2 the intersection of column 2 and row 2
layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
## show the regions that have been allocated to each plot
layout.show(2)

## divide device into two rows and two columns
## allocate figure 1 and figure 2 as above
## respect relations between widths and heights
nf <- layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE), respect = TRUE)
layout.show(nf)

## create single figure which is 5cm square
nf <- layout(matrix(1), widths = lcm(5), heights = lcm(5))
layout.show(nf)


##-- Create a scatterplot with marginal histograms -----

x <- pmin(3, pmax(-3, stats::rnorm(50)))
y <- pmin(3, pmax(-3, stats::rnorm(50)))
xhist <- hist(x, breaks = seq(-3,3,0.5), plot = FALSE)
yhist <- hist(y, breaks = seq(-3,3,0.5), plot = FALSE)
top <- max(c(xhist$counts, yhist$counts))
xrange <- c(-3, 3)
yrange <- c(-3, 3)
nf <- layout(matrix(c(2,0,1,3),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
layout.show(nf)

par(mar = c(3,3,1,1))
plot(x, y, xlim = xrange, ylim = yrange, xlab = "", ylab = "")
par(mar = c(0,3,1,1))
barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
par(mar = c(3,0,1,1))
barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)

par(def.par)  #- reset to default
}
\keyword{iplot}
\keyword{dplot}
\keyword{environment}
