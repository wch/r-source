% File src/library/grid/man/grid.group.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{grid.group}
\alias{grid.group}
\alias{groupGrob}
\alias{grid.define}
\alias{defineGrob}
\alias{grid.use}
\alias{useGrob}
\title{ Draw a Group }
\description{
  These functions define and draw one or more \dfn{groups}, where
  a group is a grob that is drawn in isolation before being
  combined with the main image.  The concept of groups
  allows for compositing operators, object reuse, and
  affine transformations (see the Details section).
}
\usage{
groupGrob(src, op = "over", dst = NULL, coords = TRUE,
          name = NULL, gp=gpar(), vp=NULL) 
grid.group(src, op = "over", dst = NULL, coords = TRUE,
           name = NULL, gp=gpar(), vp=NULL)

defineGrob(src, op = "over", dst = NULL, coords = TRUE,
           name = NULL, gp=gpar(), vp=NULL) 
grid.define(src, op = "over", dst = NULL, coords = TRUE,
            name = NULL, gp=gpar(), vp=NULL)

useGrob(group, transform=viewportTransform,
        name=NULL, gp=gpar(), vp=NULL) 

grid.use(group, transform=viewportTransform,
         name=NULL, gp=gpar(), vp=NULL) 
}
\arguments{
  \item{src}{A grob.}
  \item{op}{The name of a compositing operator (see Details).}
  \item{dst}{A grob.}
  \item{coords}{A logical indicating whether grob coordinates should be
    calculated for the group.}
  \item{group}{A character identified referring to the name of a defined group.}
  \item{transform}{A function that returns an affine transformation
    matrix; see \code{\link{viewportTransform}}.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{"gpar"}, typically the output
    from a call to the function \code{\link{gpar}}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\value{
  A grob object.
}
\details{
  In the simplest usage, we can use \code{grid.group()} to
  specify a grob to be drawn in isolation before being combined
  with the main image.  This can be different from normal drawing if,
  for example, the grob draws more than one shape and there
  is a mask currently in effect.

  Another possible use of \code{grid.group()} is to specify
  both \code{src} and \code{dst} and combine them
  using a compositing operator other than the default
  \code{"over"}, before combining the result with the main image.
  For example, if we use the \code{"dest.out"} operator then \code{dst}
  is only drawn where it is NOT overlapped by \code{src}.
  The following (extended) Porter-Duff operators are available:
  \code{"clear"}, \code{"source"}, \code{"over"},
  \code{"in"}, \code{"out"}, \code{"atop"},
  \code{"dest"}, \code{"dest.over"}, \code{"dest.in"},
  \code{"dest.out"}, \code{"dest.atop"}, \code{"xor"},
  \code{"add"}, and \code{"saturate"}.
  In addition, there are operators corresponding to PDF blend modes:
  \code{"multiply"}, \code{"screen"}, \code{"overlay"},
  \code{"darken"}, \code{"lighten"}, \code{"color.dodge"}, \code{"color.burn"},
  \code{"hard.light"}, \code{"soft.light"}, \code{"difference"}, and
  \code{"exclusion"}.
  However, even if a graphics device supports groups, it may not support all
  compositing operators;  see \code{\link{dev.capabilities}}.

  It is also possible to break the process into two steps by
  first using \code{grid.define()} to define a group and
  then \code{grid.use()} to draw the group.  This allows for
  reuse of a group (define the group once and use it several times).

  If a group is defined in one viewport and used in a different
  viewport, an implicit transformation is applied.  This could
  be a simple transformation (if the viewports are in different
  locations, but are the same size),
  or it could be more complex if the viewports are also different
  sizes or at different orientations.
  
  \strong{NOTE:} transformations occur on the graphics device
  so affect all aspects of drawing.  For example, text
  and line widths are transformed as well as locations.

  See \code{\link{viewportTransform}} for more information about
  transformations and how to customise them.
  
  Not all graphics devices support these functions: for example
  \code{xfig} and \code{pictex} do not.  For devices that do
  provide support, that support may only be partial (e.g., the
  Cairo-based devices support more compositing operators than
  the \code{pdf()} device).
}
\author{Paul Murrell}
\seealso{
  \link{Grid}
}
\examples{
## NOTE: on devices without support for groups (or masks or patterns),
##       there will only be two overlapping opaque circles 
grid.newpage()
pat <- pattern(rasterGrob(matrix(c(.5, 1, 1, .5), nrow=2),
                          width=unit(1, "cm"),
                          height=unit(1, "cm"),
                          interpolate=FALSE),
               width=unit(1, "cm"), height=unit(1, "cm"),
               extend="repeat")
grid.rect(gp=gpar(col=NA, fill=pat))
masks <- dev.capabilities()$masks
if (is.character(masks) && "luminance" \%in\% masks) {
  mask <- as.mask(rectGrob(gp=gpar(col=NA, fill="grey50")), type="luminance")
} else {
  mask <- rectGrob(gp=gpar(col=NA, fill=rgb(0,0,0,.5)))
}
pushViewport(viewport(mask=mask))
pushViewport(viewport(y=.5, height=.5, just="bottom"))
grid.circle(1:2/3, r=.45, gp=gpar(fill=2:3))
popViewport()
pushViewport(viewport(y=0, height=.5, just="bottom"))
grid.group(circleGrob(1:2/3, r=.45, gp=gpar(fill=2:3)))
popViewport()
}
\keyword{dplot}
