
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

devMask <- function(aMask, lMask) {
    support <- dev.capabilities()$masks
    if (is.character(support)) {
        if ("alpha" %in% support) {
            aMask
        } else {
            if ("luminance" %in% support) {
                as.mask(lMask, type="luminance")
            } else {
                FALSE
            }
        }
    } else {
        FALSE
    }
}

################################################################################

## Simple mask
mask <- devMask(circleGrob(r=.3, gp=gpar(fill="black")),
                circleGrob(r=.3, gp=gpar(col="white", fill="white")))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="black"))
popViewport()
HersheyLabel("solid black rectangle with circle mask", y=.1)

## VERY thin mask
mask <- devMask(circleGrob(r=.3, gp=gpar(fill=NA)),
                circleGrob(r=.3, gp=gpar(col="white", fill=NA)))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="black"))
popViewport()
HersheyLabel("solid black rectangle with circle BORDER mask", y=.1)

## Multiple grobs mask
mask <- devMask(circleGrob(x=1:3/4, y=1:3/4, r=.1, gp=gpar(fill="black")),
                circleGrob(x=1:3/4, y=1:3/4, r=.1, gp=gpar(col="white",
                                                           fill="white")))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="black"))
popViewport()
HersheyLabel("solid black rectangle with three-circle mask", y=.1)

## Mask with gradient on single grob
mask <- devMask(circleGrob(gp=gpar(col=NA,
                                   fill=radialGradient(c("black",
                                                         "transparent")))),
                circleGrob(gp=gpar(col=NA,
                                   fill=radialGradient(c("white",
                                                         "black")))))
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
mask <- devMask(gTree(children=gList(rectGrob(gp=gpar(fill="black"))),
                      vp=viewport(clip=circleGrob(r=.4))),
                gTree(children=gList(rectGrob(gp=gpar(col="white",
                                                      fill="white"))),
                      vp=viewport(clip=circleGrob(r=.4))))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="grey"))
popViewport()
HersheyLabel("rect is half width and filled grey
mask is full rect with circle clipping path
result is half width rect with rounded top and bottom", y=.1)

## Mask with a mask
mask <- devMask(gTree(children=gList(rectGrob(gp=gpar(fill="black"))),
                      vp=viewport(mask=circleGrob(r=.4,
                                                  gp=gpar(fill="black")))),
                gTree(children=gList(rectGrob(gp=gpar(col="white",
                                                      fill="white"))),
                      vp=viewport(mask=as.mask(circleGrob(r=.4,
                                                          gp=gpar(col="white",
                                                                  fill="white")),
                                               type="luminance"))))
grid.newpage()
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="grey"))
popViewport()
HersheyLabel("rect is half width and filled grey
mask is full rect with circle mask
result is half width rect with rounded top and bottom", y=.1)

## A mask from two grobs, with ONE grob making use of a clipping path 
grid.newpage()
mask <- devMask(gTree(children=gList(rectGrob(x=.25, width=.3, height=.8,
                                              gp=gpar(fill="black"),
                                              vp=viewport(clip=circleGrob(r=.4))),
                                     rectGrob(x=.75, width=.3, height=.8,
                                              gp=gpar(fill="black")))),
                gTree(children=gList(rectGrob(x=.25, width=.3, height=.8,
                                              gp=gpar(col="white",
                                                      fill="white"),
                                              vp=viewport(clip=circleGrob(r=.4))),
                                     rectGrob(x=.75, width=.3, height=.8,
                                              gp=gpar(col="white",
                                                      fill="white")))))
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
mask <- devMask(rectGrob(gp=gpar(fill="black"),
                         vp=viewport(width=.5, height=.5, clip=circleGrob())),
                rectGrob(gp=gpar(col="white", fill="white"),
                         vp=viewport(width=.5, height=.5, clip=circleGrob())))
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
mask <- devMask(circleGrob(r=.6,
                           gp=gpar(fill="black"),
                           vp=viewport(width=.5, height=.5, clip=TRUE)),
                circleGrob(r=.6,
                           gp=gpar(col="white", fill="white"),
                           vp=viewport(width=.5, height=.5, clip=TRUE)))
pushViewport(viewport(mask=mask))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("mask includes clip rect
(mask is squared circle)
push mask
rect
grey squared circle")

## Inheriting masks (between viewports)
grid.newpage()
pushViewport(viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                                   circleGrob(gp=gpar(col="white",
                                                      fill="white")))))
pushViewport(viewport())
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push mask
push again (inherit mask)
rect
grey circle")

## Restoring masks (between viewports)
grid.newpage()
pushViewport(viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                                   circleGrob(gp=gpar(col="white",
                                                      fill="white")))))
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
pushViewport(viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                                   circleGrob(gp=gpar(col="white",
                                                      fill="white")))))
grid.rect(gp=gpar(fill="grey"))
upViewport()
grid.rect(gp=gpar(fill=rgb(0,0,1,.2)))
HersheyLabel("push mask
grey circle
upViewport
page all (translucent) blue")

## downViewport()
grid.newpage()
pushViewport(viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                                   circleGrob(gp=gpar(col="white",
                                                      fill="white"))),
                      name="vp"))
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
pushViewport(viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                                   circleGrob(gp=gpar(col="white",
                                                      fill="white")))))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("push mask
rect
grey circle
(for resizing)")

## Record and replay
grid.newpage()
pushViewport(viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                                   circleGrob(gp=gpar(col="white",
                                                      fill="white")))))
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
pushViewport(viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                                   circleGrob(gp=gpar(col="white",
                                                      fill="white")))))
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

## A mask from two grobs, with ONE grob making use of a mask
grid.newpage()
mask <- devMask(gTree(children=gList(rectGrob(x=.25, width=.3, height=.8,
                                              gp=gpar(fill="black"),
                                              vp=viewport(mask=circleGrob(r=.4,
                                                                          gp=gpar(fill="black")))),
                                     rectGrob(x=.75, width=.3, height=.8,
                                              gp=gpar(fill="black")))),
                gTree(children=gList(rectGrob(x=.25, width=.3, height=.8,
                                              gp=gpar(col="white",
                                                      fill="white"),
                                              vp=viewport(mask=as.mask(circleGrob(r=.4,
                                                                                  gp=gpar(col="white", fill="white")),
                                                                       type="luminance"))),
                                     rectGrob(x=.75, width=.3, height=.8,
                                              gp=gpar(col="white",
                                                      fill="white")))))
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
    mask <- devMask(circleGrob(gp=gpar(fill="black")),
                    circleGrob(gp=gpar(col="white", fill="white")))
    setChildren(x, gList(rectGrob(gp=gpar(fill="grey"),
                                  vp=viewport(mask=mask))))
}
grid.draw(g)
HersheyLabel("custom grob class with makeContent() method
makeContent() adds rectangle with viewport
viewport has circle mask
result is grey circle")

## A mask that makes use of makeContent() method
grid.newpage()
mask <- devMask(gTree(cl="test"), gTree(cl="testLuminance"))
makeContent.test <- function(x) {
    setChildren(x, gList(circleGrob(gp=gpar(fill="black"))))
}
makeContent.testLuminance <- function(x) {
    setChildren(x, gList(circleGrob(gp=gpar(col="white", fill="white"))))
}
pushViewport(viewport(mask=mask))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("push viewport with mask
mask is grob with makeContent() method
makeContent() adds circle
draw rect
result is grey circle")

######################
## Check resource exhaustion
grid.newpage()
for (i in 1:65) {
    pushViewport(viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                                       circleGrob(gp=gpar(col="white",
                                                          fill="white")))))
    grid.rect(gp=gpar(fill="grey"))
    HersheyLabel(paste0("viewport ", i, " with mask
result is grey circle"))
    popViewport()
}

## Bug from 4.1.0 (mask should NOT be applied to pattern)
grid.newpage()
pat <- pattern(circleGrob(r=.1),
               width=.17, height=.17,
               extend="repeat")
mask <- devMask(rectGrob(0:1/2, 0:1/2, width=.5, height=.5,
                         just=c("left", "bottom"),
                         gp=gpar(fill=rgb(0,0,0,1:2/2))),
                rectGrob(0:1/2, 0:1/2, width=.5, height=.5,
                         just=c("left", "bottom"),
                         gp=gpar(col="white", fill=grey(1:2/2))))
pushViewport(viewport(mask=mask))
grid.rect(gp=gpar(fill=pat))

## Mask from text
grid.newpage()
mask <- devMask(textGrob("test", gp=gpar(cex=10)),
                textGrob("test", gp=gpar(col="white", cex=10)))
pushViewport(viewport(mask=mask))
grid.rect(width=.5, height=.5, gp=gpar(fill=linearGradient()))
popViewport()
HersheyLabel("rect filled with linear gradient
masked by text", y=.8)
    
## Text being masked
grid.newpage()
mask <- devMask(rectGrob(width=.5, height=.5,
                         gp=gpar(fill=linearGradient(c("black",
                                                       "transparent")))),
                rectGrob(width=.5, height=.5,
                         gp=gpar(fill=linearGradient(c("white",
                                                       "black")))))
grid.segments(gp=gpar(col=2, lwd=50))
pushViewport(viewport(mask=mask))
grid.text("test", gp=gpar(cex=10))
popViewport()
HersheyLabel("text with mask
mask is rect with semitransparent linear gradient", y=.8)

## Mask from raster
grid.newpage()
mask <- devMask(rasterGrob(matrix(rgb(0,0,0,1:3/4), nrow=1), interpolate=FALSE),
                rasterGrob(matrix(grey(1:3/4), nrow=1), interpolate=FALSE))
grid.segments(gp=gpar(col=2, lwd=100))
pushViewport(viewport(mask=mask))
grid.circle(r=.4, gp=gpar(fill="black"))
popViewport()
HersheyLabel("circle with mask
mask is semitransparent raster", y=.8)
    
## Raster being masked
grid.newpage()
mask <- devMask(circleGrob(r=.4, gp=gpar(col=NA, fill=rgb(0,0,0,.5))),
                circleGrob(r=.4, gp=gpar(col=NA, fill=grey(.5))))
grid.segments(gp=gpar(col=2, lwd=100))
pushViewport(viewport(mask=mask))
grid.raster(matrix(1:3/4, nrow=1), interpolate=FALSE)
popViewport()
HersheyLabel("raster with mask
mask is semitransparent circle", y=.8)
    

################################################################################
## Need to test ...

