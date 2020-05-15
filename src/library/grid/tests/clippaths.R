
library(grid)

HersheyLabel <- function(x, y=unit(.5, "npc")) {
    lines <- strsplit(x, "\n")[[1]]
    if (!is.unit(y))
        y <- unit(y, "npc")
    n <- length(lines)
    if (n > 1) {
        y <- y + unit(rev(seq(n)) - mean(seq(n)), "lines")
    }
    grid.text(lines, y=y, gp=gpar(fontfamily="HersheySans"))
}

################################################################################
## Enforce a clipping path on a viewport
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push circle clipping path
rect
grey circle")

## NORMAL clipping(!)
grid.newpage()
pushViewport(viewport(width=.5, height=.5, clip=TRUE))
grid.circle(r=.6, gp=gpar(fill="grey"))
HersheyLabel("push clipping rect
circle
squared circle")

## A slightly more complex clipping path
grid.newpage()
pushViewport(viewport(clip=circleGrob(1:2/3, r=unit(.5, "in"))))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("push two circles clipping path
rect
two circles")
    
## Clip to a polygon
grid.newpage()
pushViewport(viewport(width=.5, height=.5,
                      clip=polygonGrob(c(.2, .4, .6, .2), c(.2, .6, .4, .1))))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("push clipping path
rect
grey wedge")

## Rotated clip rect!
grid.newpage()
pushViewport(viewport(width=.5, height=.6, angle=45, clip=rectGrob()))
grid.circle(r=.6, gp=gpar(fill="grey"))
HersheyLabel("push rotated viewport
with clipping path
circle
square-sided circle")

## Clipping gradient output
## (gradient on viewport)
grid.newpage()
pushViewport(viewport(clip=circleGrob(1:2/3, r=unit(.5, "in")),
                      gp=gpar(fill=linearGradient())))
grid.rect()
popViewport()
HersheyLabel("push clipping path
gradient on viewport
two circles (one gradient)")

## Clipping gradient output
## (gradient on grob)
grid.newpage()
pushViewport(viewport(clip=circleGrob(1:2/3, r=unit(.5, "in"))))
grid.rect(gp=gpar(fill=linearGradient()))
popViewport()
HersheyLabel("push clipping path
rect with gradient
two circles (one gradient)")

## Inheriting clipping paths (between viewports)
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
pushViewport(viewport())
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push clipping path
push again (inherit clip path)
rect
grey circle")

## Restoring clipping paths (between viewports)
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
pushViewport(viewport())
pushViewport(viewport())
upViewport()
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push clipping path
push again (inherit clip path)
push again (inherit clip path)
up (restore inherited clip path)
rect
grey circle")

## Revisiting clipping on a viewport
## upViewport()
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
upViewport()
grid.rect(gp=gpar(fill=rgb(0,0,1,.2)))
HersheyLabel("push clipping path
grey circle
upViewport
page all (translucent) blue")

## downViewport()
grid.newpage()
pushViewport(viewport(clip=circleGrob(), name="vp"))
grid.rect(height=.5, gp=gpar(fill="grey"))
upViewport()
downViewport("vp")
grid.rect(gp=gpar(fill=rgb(0,0,1,.2)))
HersheyLabel("push clipping path
rounded rect
upViewport
downViewport
blue (translucent) circle")

## clip rect to clip path back to clip rect
grid.newpage()
pushViewport(viewport(width=.5, height=.5, clip=TRUE))
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
upViewport()
grid.circle(r=.6, gp=gpar(fill=rgb(0,0,1,.2)))
HersheyLabel("push clipping rect
push clipping path
grey circle
upViewport
squared (translucent) blue circle")

## clip path to clip rect back to clip path
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
pushViewport(viewport(width=.5, height=.5, clip=TRUE))
grid.circle(r=.6, gp=gpar(fill="grey"))
upViewport()
grid.rect(gp=gpar(fill=rgb(0,0,1,.2)))
HersheyLabel("push clipping path
push clipping rect
squared circle
upViewport
grey (translucent) blue circle")

## A clipping path that itself makes use of a clipping path !?
grid.newpage()
clipPath <- rectGrob(vp=viewport(width=.5, height=.5, clip=circleGrob()))
pushViewport(viewport(clip=clipPath))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("clip path includes clip path
(clip path is circle)
push clipping path
rect
small grey circle")

## A clipping path that itself makes use of a rectangular clipping !?
grid.newpage()
clipPath <- circleGrob(r=.6,
                       gp=gpar(fill="grey"),
                       vp=viewport(width=.5, height=.5, clip=TRUE))
pushViewport(viewport(clip=clipPath))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("clip path includes clip rect
(clip path is squared circle)
push clipping path
rect
squared circle")

## Clipping path makes the clipping region BIGGER
grid.newpage()
pushViewport(viewport(width=.5, height=.5, clip=TRUE))
grid.rect()
pushViewport(viewport(clip=circleGrob(r=.6)))
grid.rect(width=1.2, height=1.2, gp=gpar(fill=rgb(0,0,1,.2)))
HersheyLabel("push clip rect (small)
rect
push clip path (bigger)
rect (big)
blue (translucent) circle")

## Clipping path makes the clipping region BIGGER
## (even when clipping path contains viewport)
grid.newpage()
pushViewport(viewport(width=.5, height=.5, clip=TRUE))
grid.rect()
pushViewport(viewport(clip=circleGrob(r=.6, vp=viewport())))
grid.rect(width=1.2, height=1.2, gp=gpar(fill=rgb(0,0,1,.2)))
HersheyLabel("push clip rect (small)
rect
push clip path with viewport (bigger)
rect (big)
blue (translucent) circle")


######################################
## Replaying the graphics display list

## Resizing device
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push clip path
rect
grey circle
(for resizing)")

## Record and replay
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
x <- recordPlot()
HersheyLabel("push clip path
rect
grey circle
(for recording)")
print(x)
HersheyLabel("push clip path
rect
record plot
replay plot
grey circle")


######################################
## Test of 'grid' display list

## Grabbing a grob with clipping
## (replaying the 'grid' display list)
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push clip path
rect
grey circle
(for grid.grab)")
x <- grid.grab()
grid.newpage()
grid.draw(x)
HersheyLabel("push clip path
rect
grey circle
grid.grab
grid.draw
grey circle")



######################
## Check resource exhaustion
options(warn=2)
grid.newpage()
for (i in 1:21) {
    result <- try(pushViewport(viewport(clip=circleGrob())))
    if (!inherits(result, "try-error")) {
        grid.rect(gp=gpar(fill="grey"))
        HersheyLabel(paste0("viewport ", i, " with clip path
runs out after 20"))
    }
}


## A clipping path from two grobs, with ONE grob making use of a clipping path 
grid.newpage()
clipPath <- gTree(children=gList(rectGrob(x=.25, width=.3, height=.8,
                                          vp=viewport(clip=circleGrob(r=.4))),
                                 rectGrob(x=.75, width=.3, height=.8)))
pushViewport(viewport(clip=clipPath))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("clip path is two grobs, ONE with its own clip path
(second clip path is circle)
push clipping path
rect
two slices of circle
(SVG says right slice should be rect)")


######################
## NOT YET WORKING


## Clipping path from text (Pango)

## Clipping path from text ("Toy text")

## A clipping path on a grob
## (defined relative to viewport that grob is in)

## A clipping path on a gTree
## (defined relative to viewport that gTree is in)

## Clipping path on viewport in vp of grob

## Clipping path on viewport in vp of gTree

## A clipping path that makes use of makeContent() method


