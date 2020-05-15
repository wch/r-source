
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

## Clipping path with a mask
clip <- gTree(children=gList(rectGrob(gp=gpar(fill="black"))),
              vp=viewport(mask=circleGrob(r=.4, gp=gpar(fill="black"))))
grid.newpage()
pushViewport(viewport(clip=clip))
grid.rect(width=.5, gp=gpar(fill="grey"))
popViewport()
HersheyLabel("rect is half width and filled grey
clipping path is full rect with circle mask
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
HersheyLabel("mask is two grobs, ONE with its own clip path
(second clip path is circle)
push clipping path
rect
result is one slice of circle and one rectangle")


################################################################################
## Need to test ...

##   clip with mask

##   inheriting mask
##   pushing and popping masks
##   pushing and popping no masks

##   display list replays
##   display resizes
##   grid.grab()
##   recordPlot()/replayPlot()
