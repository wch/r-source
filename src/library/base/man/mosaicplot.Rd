\name{mosaicplot}
\alias{mosaicplot}
\alias{mosaicplot.default}
\alias{mosaicplot.formula}
\title{Mosaic Plots}
\description{Plots a mosaic on the current graphics device.}
\usage{
mosaicplot(x, \dots)
mosaicplot.default(X, main = NULL, xlab = NULL, ylab = NULL,
                   sort = NULL, off = NULL, dir = NULL,
                   color = FALSE)
mosaicplot.formula(formula, data = NULL, subset, na.action, \dots)
}
\arguments{
  \item{x}{an \R object.}
  \item{X}{a contingency table, with optional category labels
    specified in the \code{dimnames(x)} attribute.  The table is
    best created by the \code{table()} command, which produces an
    object of type array.}
  \item{main}{character string for the mosaic title.}
  \item{xlab,ylab}{x- and y-axis labels; by default,
    \code{names(dimnames(X))}.}
  \item{sort}{vector ordering of the variables, containing a
    permutation of the integers \code{1:length(dim(x))} (the
    default).}
  \item{off}{vector of offsets to determine percentage spacing at each
    level of the mosaic (appropriate values are between 0 and 20,
    and the default is 10 at each level).  There should be one
    offset for each dimension of the contingency table.}
  \item{dir}{vector of split directions (\code{"v"} for vertical and
    \code{"h"} for horizontal) for each level of the mosaic, one
    direction for each dimension of the contingency table.  The
    default consists of alternating directions, beginning with a
    vertical split.}
  \item{color}{(\code{TRUE} or vector of integer colors) for color
    shading or (\code{FALSE}, the default) for empty boxes with no
    shading.}
  \item{formula}{a formula, such as \code{y ~ x}.}
  \item{data}{a data.frame (or list) from which the variables in
    \code{formula} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.}
  \item{\dots}{further arguments to the default mosaicplot method.}
}
\details{
  This is a generic function.  It currently has a default method
  (\code{\link{mosaicplot.default}}) and a formula interface
  (\code{\link{mosaicplot.formula}}).

  See Emerson (1998) for more information and a case study with
  television viewer data from Nielsen Media Research.
}
\author{
  S-PLUS original by John Emerson \email{emerson@stat.yale.edu}.
  Slightly modified for \R by KH.
}
\references{
  Hartigan, J.A., and Kleiner, B. (1984)
  A mosaic of television ratings. \emph{The American Statistician},
  \bold{38}, 32--35.

  Emerson, J. W. (1998)
  Mosaic displays in S-PLUS: a general implementation and a case study.
  \emph{Statistical Computing and Graphics Newsletter (ASA)},
  \bold{9}, 1, 17--23.

  Friendly, M. (1994)
  Mosaic displays for multi-way contingency tables.
  \emph{Journal of the American Statistical Association}, \bold{89},
  190--200.

  The home page of Michael Friendly
  (\url{http://hotspur.psych.yorku.ca/SCS/friendly.html}) provides
  information on various aspects of graphical methods for analyzing
  categorical data, including mosaic plots.
}
\examples{
data(Titanic)
mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)

## Formula interface: visualize crosstabulation of numbers of gears and
## carburettors in Motor Trend car data.
data(mtcars)
mosaicplot(~ gear + carb, data = mtcars, color = TRUE)
}
\keyword{hplot}
