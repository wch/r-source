% File src/library/grid/man/grid.xaxis.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{grid.xaxis}
\alias{grid.xaxis}
\alias{xaxisGrob}
\title{Draw an X-Axis}
\description{
  These functions create and draw an x-axis.
}
\usage{
grid.xaxis(at = NULL, label = TRUE, main = TRUE,
           edits = NULL, name = NULL,
           gp = gpar(), draw = TRUE, vp = NULL)

xaxisGrob(at = NULL, label = TRUE, main = TRUE,
          edits = NULL, name = NULL,
          gp = gpar(), vp = NULL)
}
\arguments{
  \item{at}{A numeric vector of x-value locations for the tick marks.}
  \item{label}{A logical value indicating whether to draw the labels
    on the tick marks, or an expression or character vector which
    specify the labels to use.  If not logical, must be the same
    length as the \code{at} argument.}
  \item{main}{A logical value indicating whether to draw the axis
    at the bottom (\code{TRUE}) or at the top (\code{FALSE}) of the
    viewport.}
  \item{edits}{A \I{gEdit} or \I{gEditList} containing edit operations
    to apply (to the children of the axis)
    when the axis is first created and during redrawing
    whenever \code{at} is \code{NULL}.
    }
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{"gpar"}, typically the output
    from a call to the function \code{\link{gpar}}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or \code{NULL}).}
}
\details{
  Both functions create an \I{xaxis} grob (a graphical object describing an
  \I{xaxis}), but only \code{grid.xaxis}
  draws the \I{xaxis} (and then only if \code{draw} is \code{TRUE}).
}
\value{
  An \I{xaxis} grob.  \code{grid.xaxis} returns the value invisibly.
}
\section{Children}{
  If the \code{at} slot of an \I{xaxis} grob is not \code{NULL} then
  the \I{xaxis} will have the following children:
  \describe{
    \item{major}{ representing the line at the base of the
    tick marks.}
    \item{ticks}{representing the tick marks.}
    \item{labels}{representing the tick labels.}
  }

  If the \code{at} slot is \code{NULL} then there are no children
  and ticks are drawn based on the current viewport scale.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{grid.yaxis}}
}
\keyword{dplot}
