
library(grid)

## Enforce a clipping path on a viewport
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))

## NORMAL clipping(!)
grid.newpage()
pushViewport(viewport(width=.5, height=.5, clip=TRUE))
grid.circle(r=.6, gp=gpar(fill="grey"))

## A slightly more complex clipping path
grid.newpage()
pushViewport(viewport(clip=circleGrob(1:2/3, r=unit(.5, "in"))))
grid.rect(gp=gpar(fill="grey"))

## Rotated clip rect!
grid.newpage()
pushViewport(viewport(width=.5, height=.6, angle=45, clip=rectGrob()))
grid.circle(r=.6, gp=gpar(fill="grey"))

## Clipping gradient output
## (gradient on viewport)
grid.newpage()
pushViewport(viewport(clip=circleGrob(1:2/3, r=unit(.5, "in")),
                      gp=gpar(fill=linearGradient())))
grid.rect()

######################
## NOT YET WORKING

## Inheriting clipping paths (between viewports)

## Mix of (viewports with) clipping paths and (viewports with) clipping rects

## Clipping path from text (Pango)

## Clipping path from text ("Toy text")

## Clipping gradient output
## (gradient on grob)
grid.newpage()
pushViewport(viewport(clip=circleGrob(1:2/3, r=unit(.5, "in"))))
grid.rect(gp=gpar(fill=linearGradient()))

## Revisiting a clipping path on a viewport
## upViewport()
## downViewport()

## A clipping path on a grob
## (defined relative to viewport that grob is in)

## A clipping path on a gTree
## (defined relative to viewport that gTree is in)

## A clipping path that makes use of makeContent() method

## A clipping path that itself makes use of a clipping path !?

## A clipping path that itself makes use of a rectangular clipping !?
grid.newpage()
clipPath <- circleGrob(r=.6, vp=viewport(width=.5, height=.5, clip=TRUE))
pushViewport(viewport(clip=clipPath))
grid.rect(gp=gpar(fill="grey"))

## Replaying the graphics display list

## Grabbing a grob with clipping
## (replaying the 'grid' display list)
