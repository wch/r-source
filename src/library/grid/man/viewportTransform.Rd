% File src/library/grid/man/viewportTransform.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{viewportTransform}
\alias{viewportTransform}
\alias{viewportTranslate}
\alias{viewportScale}
\alias{viewportRotate}
\alias{defnTranslate}
\alias{defnScale}
\alias{defnRotate}
\alias{useTranslate}
\alias{useScale}
\alias{useRotate}
\alias{groupTranslate}
\alias{groupScale}
\alias{groupRotate}
\alias{groupShear}
\alias{groupFlip}
\title{ Define a Group Transformation }
\description{
  These functions define the transformation that will be applied
  when a \code{grid.define()}d group is \code{grid.use()}d.
}
\usage{
viewportTransform(group, shear=groupShear(), flip=groupFlip(), device=TRUE) 
viewportTranslate(group, device=TRUE)
viewportScale(group, device=TRUE)
viewportRotate(group, device=TRUE)
defnTranslate(group, inverse=FALSE, device=TRUE)
defnScale(group, inverse=FALSE)
defnRotate(group, inverse=FALSE, device=TRUE)
useTranslate(inverse=FALSE, device=TRUE)
useScale(inverse=FALSE)
useRotate(inverse=FALSE, device=TRUE)
groupTranslate(dx=0, dy=0)
groupRotate(r=0, device=TRUE)
groupScale(sx=1, sy=1)
groupShear(sx=0, sy=0)
groupFlip(flipX=FALSE, flipY=FALSE)
}
\arguments{
  \item{group}{The group that is being transformed.}
  \item{inverse}{A logical indicating whether we want the
    forward or backward transformation.}
  \item{shear}{An affine transformation matrix that describes
    a shear transformation.}
  \item{flip}{An affine transformation matrix that describes
    a scaling inversion.}
  \item{dx, dy}{The translation to apply.}
  \item{r}{The rotation to apply.}
  \item{sx, sy}{The scaling (or shear) to apply.}
  \item{flipX, flipY}{Whether to negate the x-scaling or y-scaling
    (logical).}
  \item{device}{A logical indicating whether transformation should be
    relative to the device or relative to the current viewport.}
}
\value{
  An affine transformation matrix.
}
\details{
  The \code{viewport*()} functions are not called directly.  They
  are passed as the \code{transform} argument to
  \code{\link{grid.use}}.

  The \code{defn*()} and \code{use*()} functions are also not called
  directly, but can be useful to create custom transformation
  functions.  For example, see the source code for
  \code{viewportTransform}.

  The \code{group*()} functions generate basic affine transformation
  matrices and may also be useful to create custom transformation
  functions.  For example, the \code{groupShear()} function can be
  used to specify a shear transform to \code{viewportTransform()}.

  It is also possible to define any function that returns a 3x3 matrix
  (as long as the last column contains 0, 0, and 1)
  and use it as the \code{transform} argument to
  \code{\link{grid.use}}, but the results will probably be
  device-dependent, and may be \emph{very} difficult
  to predict.  The function will be called with two arguments:
  \code{group} and \code{device}.
}
\author{Paul Murrell}
\seealso{
  \link{Grid}
}
\examples{
## NOTE: on devices without support for groups nothing will be drawn
grid.newpage()
## Define and use group in same viewport
pushViewport(viewport(width=.2, height=.2))
grid.define(circleGrob(gp=gpar(lwd=5)), name="circle")
grid.use("circle")
popViewport()
## Use group in viewport that is translated and scaled
pushViewport(viewport(x=.2, y=.2, width=.1, height=.1))
grid.use("circle")
popViewport()
## Use group in viewport that is translated and scaled
## BUT only make use of the translation
pushViewport(viewport(x=.2, y=.8, width=.1, height=.1))
grid.use("circle", transform=viewportTranslate)
popViewport()
## Use group in viewport that is translated and scaled
## unevenly (distorted)
pushViewport(viewport(x=.8, y=.7, width=.2, height=.4))
grid.use("circle")
popViewport()
}
\keyword{dplot}
