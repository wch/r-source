
library(grid)

## Enforce a clipping path on a viewport
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
grid.text("push clipping path
grey circle")

## NORMAL clipping(!)
grid.newpage()
pushViewport(viewport(width=.5, height=.5, clip=TRUE))
grid.circle(r=.6, gp=gpar(fill="grey"))
grid.text("push clipping rect
squared circle")

## A slightly more complex clipping path
grid.newpage()
pushViewport(viewport(clip=circleGrob(1:2/3, r=unit(.5, "in"))))
grid.rect(gp=gpar(fill="grey"))
popViewport()
grid.text("push clipping path
two circles")
    
## Clip to a polygon
grid.newpage()
pushViewport(viewport(width=.5, height=.5,
                      clip=polygonGrob(c(.2, .4, .6, .2), c(.2, .6, .4, .1))))
grid.rect(gp=gpar(fill="grey"))
popViewport()
grid.text("push clipping path
grey wedge")

## Rotated clip rect!
grid.newpage()
pushViewport(viewport(width=.5, height=.6, angle=45, clip=rectGrob()))
grid.circle(r=.6, gp=gpar(fill="grey"))
grid.text("push rotated viewport
with clipping path
squared circle")

## Clipping gradient output
## (gradient on viewport)
grid.newpage()
pushViewport(viewport(clip=circleGrob(1:2/3, r=unit(.5, "in")),
                      gp=gpar(fill=linearGradient())))
grid.rect()
popViewport()
grid.text("push clipping path
and gradient
two circles (one gradient)")

## Revisiting clipping on a viewport
## upViewport()
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
upViewport()
grid.rect(gp=gpar(fill=rgb(0,0,0,.2)))
grid.text("push clipping path
grey circle
upViewport
page all grey")

## downViewport()
grid.newpage()
pushViewport(viewport(clip=circleGrob(), name="vp"))
grid.rect(height=.5, gp=gpar(fill="grey"))
upViewport()
downViewport("vp")
grid.rect(gp=gpar(fill=rgb(0,0,0,.2)))
grid.text("push clipping path
rounded rect
upViewport
downViewport
grey circle")

## clip rect to clip path back to clip rect
grid.newpage()
pushViewport(viewport(width=.5, height=.5, clip=TRUE))
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
upViewport()
grid.circle(r=.6, gp=gpar(fill=rgb(0,0,0,.2)))
grid.text("push clipping rect
push clipping path
grey circle
upViewport
squared circle")

## clip path to clip rect back to clip path
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
pushViewport(viewport(width=.5, height=.5, clip=TRUE))
grid.circle(r=.6, gp=gpar(fill="grey"))
upViewport()
grid.rect(gp=gpar(fill=rgb(0,0,0,.2)))
grid.text("push clipping path
push clipping rect
squared circle
upViewport
grey circle")



######################
## Check resource exhaustion
for (i in 1:21) {
    grid.newpage()
    pushViewport(viewport(clip=circleGrob()))
    grid.rect(gp=gpar(fill="grey"))
}



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
