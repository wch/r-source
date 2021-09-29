% File src/library/grid/man/grid.stroke.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{grid.stroke}
\alias{grid.stroke}
\alias{strokeGrob}
\alias{strokeGrob.grob}
\alias{strokeGrob.GridPath}
\alias{grid.fill}
\alias{fillGrob}
\alias{fillGrob.grob}
\alias{fillGrob.GridPath}
\alias{grid.fillStroke}
\alias{fillStrokeGrob}
\alias{fillStrokeGrob.grob}
\alias{fillStrokeGrob.GridPath}
\alias{as.path}
\title{ Stroke or Fill a Path }
\description{
  These functions stroke (draw a line along the border) or
  fill (or both) a path, where the path is defined by
  a grob.
}
\usage{
strokeGrob(x, \dots)
\method{strokeGrob}{grob}(x, name=NULL, gp=gpar(), vp=NULL, \dots)
\method{strokeGrob}{GridPath}(x, name=NULL, vp=NULL, \dots)
grid.stroke(\dots)
fillGrob(x, \dots)
\method{fillGrob}{grob}(x, rule=c("winding", "evenodd"),
         name=NULL, gp=gpar(), vp=NULL, \dots)
\method{fillGrob}{GridPath}(x, name=NULL, vp=NULL, \dots)
grid.fill(\dots)
fillStrokeGrob(x, \dots)
\method{fillStrokeGrob}{grob}(x, rule=c("winding", "evenodd"),
               name=NULL, gp=gpar(), vp=NULL, \dots)
\method{fillStrokeGrob}{GridPath}(x, name=NULL, vp=NULL, \dots)
grid.fillStroke(\dots)
as.path(x, gp=gpar(), rule=c("winding", "evenodd"))
}
\arguments{
  \item{x}{A grob or the result of a call to \code{as.path()}.}
  \item{rule}{A fill rule.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{"gpar"}, typically the output
    from a call to the function \code{\link{gpar}}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
  \item{\dots}{Arguments to \code{grid.*()} passed on to \code{*Grob()},
    or additional arguments passed on to methods.}
}
\value{
  A grob object.
}
\details{
  A path is defined by the shapes that the grob given in \code{x}
  would draw.  The grob only contributes to the outline of the
  path;  graphical parameter settings such as line colour and fill
  are ignored.
  
  \code{grid.stroke()} will only ever draw the border (even when
  a fill is specified).

  \code{grid.fill()} will only ever fill the path (even when a line
  colour is specified).

  A stroke will only ever happen if a non-transparent line colour
  is specified and a fill will only ever happen if a non-transparent
  fill is specified.

  \code{as.path()} allows graphical parameter settings and a fill
  rule to be associated with a grob.  This can be useful when
  specifying a clipping path for a viewport (see \code{\link{viewport}}).

  Not all graphics devices support these functions: for example
  \code{xfig} and \code{pictex} do not.  
}
\author{Paul Murrell}
\seealso{
  \link{Grid}
}
\examples{
## NOTE: on devices without support for stroking and filling
##       nothing will be drawn
grid.newpage()
grid.stroke(textGrob("hello", gp=gpar(cex=10)))
grid.fill(circleGrob(1:2/3, r=.3), gp=gpar(fill=rgb(1,0,0,.5)))
}
\keyword{dplot}
