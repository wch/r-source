
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

## Simple mask
mask <- circleGrob(r=.3, gp=gpar(fill="black"))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="black"))
popViewport()
HersheyLabel("solid black rectangle with circle mask", y=.1)

## VERY thin mask
mask <- circleGrob(r=.3, gp=gpar(fill=NA))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="black"))
popViewport()
HersheyLabel("solid black rectangle with circle BORDER mask", y=.1)

## Multiple grobs mask
mask <- circleGrob(x=1:3/4, y=1:3/4, r=.1, gp=gpar(fill="black"))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="black"))
popViewport()
HersheyLabel("solid black rectangle with three-circle mask", y=.1)

## Mask with gradient on single grob
mask <- circleGrob(gp=gpar(col=NA,
                           fill=radialGradient(c("black", "transparent"))))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="black"))
popViewport()
HersheyLabel("solid black rectangle with radial gradient mask", y=.1)

## Mask with gradient on multiple grobs
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(x=1:2/3, width=.2, gp=gpar(fill="black"))
popViewport()
HersheyLabel("two solid black rectangles with radial gradient mask", y=.1)

## Mask with clipping path
mask <- gTree(children=gList(rectGrob(gp=gpar(fill="black"))),
              vp=viewport(clip=circleGrob(r=.4)))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="grey"))
popViewport()
HersheyLabel("rect is half width and filled grey
mask is full rect with circle clipping path
result is half width rect with rounded top and bottom", y=.1)

## Mask with a mask
mask <- gTree(children=gList(rectGrob(gp=gpar(fill="black"))),
              vp=viewport(mask=circleGrob(r=.4, gp=gpar(fill="black"))))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="grey"))
popViewport()
HersheyLabel("rect is half width and filled grey
mask is full rect with circle mask
result is half width rect with rounded top and bottom", y=.1)

## A mask from two grobs, with ONE grob making use of a clipping path 
grid.newpage()
mask <- gTree(children=gList(rectGrob(x=.25, width=.3, height=.8,
                                      gp=gpar(fill="black"),
                                      vp=viewport(clip=circleGrob(r=.4))),
                             rectGrob(x=.75, width=.3, height=.8,
                                      gp=gpar(fill="black"))))
pushViewport(viewport(mask=mask))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("mask is two grobs, ONE with its own (circle) clip path
push mask
rect
result is one slice of circle and one rectangle")

## A mask that is equivalent to ...
## A clipping path that itself makes use of a clipping path !?
grid.newpage()
mask <- rectGrob(gp=gpar(fill="black"),
                 vp=viewport(width=.5, height=.5, clip=circleGrob()))
pushViewport(viewport(mask=mask))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("mask includes clip path
(clip path is circle)
push mask
rect
small grey circle")

## A mask that is equivalent to ...
## A clipping path that itself makes use of a rectangular clipping !?
grid.newpage()
mask <- circleGrob(r=.6,
                   gp=gpar(fill="black"),
                   vp=viewport(width=.5, height=.5, clip=TRUE))
pushViewport(viewport(mask=mask))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("mask includes clip rect
(mask is squared circle)
push mask
rect
grey squared circle")

## Inheriting masks (between viewports)
grid.newpage()
pushViewport(viewport(mask=circleGrob(gp=gpar(fill="black"))))
pushViewport(viewport())
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push mask
push again (inherit mask)
rect
grey circle")

## Restoring masks (between viewports)
grid.newpage()
pushViewport(viewport(mask=circleGrob(gp=gpar(fill="black"))))
pushViewport(viewport())
pushViewport(viewport())
upViewport()
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push mask
push again (inherit mask)
push again (inherit mask)
up (restore inherited mask)
rect
grey circle")

## Revisiting mask on a viewport
## upViewport()
grid.newpage()
pushViewport(viewport(mask=circleGrob(gp=gpar(fill="black"))))
grid.rect(gp=gpar(fill="grey"))
upViewport()
grid.rect(gp=gpar(fill=rgb(0,0,1,.2)))
HersheyLabel("push mask
grey circle
upViewport
page all (translucent) blue")

## downViewport()
grid.newpage()
pushViewport(viewport(mask=circleGrob(gp=gpar(fill="black")), name="vp"))
grid.rect(height=.5, gp=gpar(fill="grey"))
upViewport()
downViewport("vp")
grid.rect(gp=gpar(fill=rgb(0,0,1,.2)))
HersheyLabel("push mask
rounded rect
upViewport
downViewport
blue (translucent) circle")

######################################
## Replaying the graphics display list

## Resizing device
grid.newpage()
pushViewport(viewport(mask=circleGrob(gp=gpar(fill="black"))))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push mask
rect
grey circle
(for resizing)")

## Record and replay
grid.newpage()
pushViewport(viewport(mask=circleGrob(gp=gpar(fill="black"))))
grid.rect(gp=gpar(fill="grey"))
x <- recordPlot()
HersheyLabel("push mask
rect
grey circle
(for recording)")
print(x)
HersheyLabel("push mask
rect
record plot
replay plot
grey circle")

######################################
## Test of 'grid' display list

## Grabbing a grob with mask
## (replaying the 'grid' display list)
grid.newpage()
pushViewport(viewport(mask=circleGrob(gp=gpar(fill="black"))))
grid.rect(gp=gpar(fill="grey"))
x <- grid.grab()
HersheyLabel("push mask
rect
grey circle
(for grid.grab)")
grid.newpage()
grid.draw(x)
HersheyLabel("push mask
rect
grey circle
grid.grab
grid.draw
grey circle")

######################
## Check resource exhaustion
options(warn=1)
grid.newpage()
for (i in 1:21) {
    pushViewport(viewport(mask=circleGrob(gp=gpar(fill="black"))))
    grid.rect(gp=gpar(fill="grey"))
    HersheyLabel(paste0("viewport ", i, " with mask
runs out after 20
result is grey rect"))
    popViewport()
}
options(warn=0)

## A mask from two grobs, with ONE grob making use of a mask
grid.newpage()
mask <- gTree(children=gList(rectGrob(x=.25, width=.3, height=.8,
                                      gp=gpar(fill="black"),
                                      vp=viewport(mask=circleGrob(r=.4,
                                                                  gp=gpar(fill="black")))),
                             rectGrob(x=.75, width=.3, height=.8,
                                      gp=gpar(fill="black"))))
pushViewport(viewport(mask=mask))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("mask is two grobs, ONE with its own (circle) mask
push mask
rect
result is one slice of circle and one rectangle")

## A mask within a makeContent() method
grid.newpage()
g <- gTree(cl="test")
makeContent.test <- function(x) {
    setChildren(x, gList(rectGrob(gp=gpar(fill="grey"),
                                  vp=viewport(mask=circleGrob(gp=gpar(fill="black"))))))
}
grid.draw(g)
HersheyLabel("custom grob class with makeContent() method
makeContent() adds rectangle with viewport
viewport has circle mask
result is grey circle")

## A mask that makes use of makeContent() method
grid.newpage()
mask <- gTree(cl="test")
makeContent.test <- function(x) {
    setChildren(x, gList(circleGrob(gp=gpar(fill="black"))))
}
pushViewport(viewport(mask=mask))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("push viewport with mask
mask is grob with makeContent() method
makeContent() adds circle
draw rect
result is grey circle")

################################################################################
## Need to test ...

