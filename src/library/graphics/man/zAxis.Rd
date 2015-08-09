% File src/library/graphics/man/zAxis.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{Axis}
\alias{Axis}
\title{Generic Function to Add an Axis to a Plot}
\description{
  Generic function to add a suitable axis to the current plot.
}
\usage{
Axis(x = NULL, at = NULL, \dots, side, labels = NULL)
}
\arguments{
  \item{x}{an object which indicates the range over which an axis should
    be drawn}
  \item{at}{the points at which tick-marks are to be drawn.}
  \item{side}{an integer specifying which side of the plot the axis is
    to be drawn on.  The axis is placed as follows: 1=below,
    2=left, 3=above and 4=right.}
  \item{labels}{this can either be a logical value specifying whether
    (numerical) annotations are to be made at the tickmarks, or a
    character or expression vector of labels to be placed at the
    tickpoints. If this is specified as a character or expression
    vector, \code{at} should be supplied and they should be the same
    length.}
  \item{\dots}{Arguments to be passed to methods and perhaps then to
    \code{\link{axis}}.}
}
\value{
  The numeric locations on the axis scale at which tick marks were drawn
  when the plot was first drawn (see \sQuote{Details}).

  This function is usually invoked for its side effect, which is to add
  an axis to an already existing plot.
}
\details{
  This is a generic function.  It works in a slightly non-standard way:
  if \code{x} is supplied and non-NULL it dispatches on \code{x},
  otherwise if \code{at} is supplied and non-NULL it dispatches on \code{at},
  and the default action is to call \code{\link{axis}}, omitting argument
  \code{x}.

  The idea is that for plots for which either or both of the axes are
  numerical but with a special interpretation, the standard plotting
  functions (including \code{\link{boxplot}}, \code{\link{contour}},
  \code{\link{coplot}}, \code{\link{filled.contour}},
  \code{\link{pairs}}, \code{\link{plot.default}}, \code{\link{rug}} and
  \code{\link{stripchart}}) will set up user coordinates and \code{Axis}
  will be called to label them appropriately.

  There are \code{"Date"} and \code{"POSIXt"} methods which can pass an
  argument \code{format} on to the appropriate \code{axis} method (see
  \code{\link{axis.POSIXct}}).
}
\seealso{
  \code{\link{axis}}.
}
\keyword{aplot}
