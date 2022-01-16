
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
## Check for outlawed behaviour
## (clipping and masks are turned off during clipping path resolution)

grid.newpage()
path <- circleGrob(r=.6,
                   vp=viewport(width=.5, height=.5, clip=TRUE))
pushViewport(viewport(clip=path))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("clipping path is circle with viewport with clip=TRUE
AND circle is larger than viewport
BUT viewport clipping is ignored within clipping path
SO clipping path is just circle
draw rect
result is grey circle")

grid.newpage()
path <- circleGrob(vp=viewport(clip=rectGrob(width=.8, height=.8)))
pushViewport(viewport(clip=path))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("clip path is circle with viewport with clip path
viewport clip path is rect that squares off the circle
BUT clip paths are ignored within clipping path (with warning)
SO clipping path is just circle
draw rect
result is grey circle")

grid.newpage()
path <- circleGrob(vp=viewport(mask=rectGrob(width=.8, height=.8,
                                             gp=gpar(fill="black"))))
pushViewport(viewport(clip=path))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("clip path is circle with viewport with mask
viewport mask is rect that squares off the circle
BUT masks are ignored within clipping path (with warning)
SO clipping path is just circle
draw rect
result is grey circle")

grid.newpage()
path <- circleGrob(vp=viewport(mask=rectGrob(width=.8, height=.8,
                                             gp=gpar(fill="black"))))
pushViewport(viewport(clip=path, name="test"))
upViewport()
downViewport("test")
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("same as previous test
EXCEPT that push viewport then pop then down
(and only push should warn)
draw rect
result is grey circle")

## A clipping path within a makeContent() method
grid.newpage()
g <- gTree(cl="test")
makeContent.test <- function(x) {
    setChildren(x, gList(rectGrob(gp=gpar(fill="grey"),
                                  vp=viewport(clip=circleGrob()))))
}
grid.draw(g)
HersheyLabel("custom grob class with makeContent() method
makeContent() adds rectangle with viewport
viewport has circle clip path
result is grey circle")

## A clipping path that makes use of makeContent() method
grid.newpage()
path <- gTree(cl="test")
makeContent.test <- function(x) {
    setChildren(x, gList(circleGrob()))
}
pushViewport(viewport(clip=path))
grid.rect(gp=gpar(fill="grey"))
popViewport()
HersheyLabel("push viewport with clip path
clip path is grob with makeContent() method
makeContent() adds circle
draw rect
result is grey circle")


## save()/load() a recordedPlot containing a clipping path
grid.newpage()
pushViewport(viewport(clip=circleGrob()))
grid.rect(gp=gpar(fill="grey"))
x <- recordPlot()
HersheyLabel("push circle clipping path
rect
grey circle
(for save(recordPlot()))")
f <- tempfile()
saveRDS(x, file=f)
grid.newpage()
y <- readRDS(f)
replayPlot(y)
HersheyLabel("push circle clipping path
rect
grey circle
saveRDS(recordPlot())
replayPlot(readRDS())")

################################################################################
## Clipping paths from text

grid.newpage()
tg <- textGrob("testing", gp=gpar(fontface="bold", cex=4))
pushViewport(viewport(clip=tg))
grid.rect(gp=gpar(fill="grey"))
grid.rect(width=.1, gp=gpar(fill="2"))
popViewport()
HersheyLabel("clipping path from text
grey rect clipped to text
thin red rect (black border) also clipped to text", y=.9)

grid.newpage()
gt <- gTree(children=gList(circleGrob(),
                           textGrob("testing", gp=gpar(fontface="bold", cex=4)),
                           rectGrob(width=.8, height=.5)))
pushViewport(viewport(clip=as.path(gt, rule="evenodd")))
grid.rect(gp=gpar(fill="grey"))
grid.rect(width=.1, gp=gpar(fill="2"))
popViewport()
HersheyLabel("clipping path based on circle and text and rect
with even-odd rule
draw large grey rect and thin red rect
both clipped to text and space between circle and rect
(PDF will NOT include text in clipping path", y=.85)

grid.newpage()
gt <- gTree(children=gList(textGrob("testing", gp=gpar(fontface="bold", cex=4)),
                           circleGrob(),
                           rectGrob(width=.8, height=.5)))
pushViewport(viewport(clip=as.path(gt, rule="evenodd")))
grid.rect(gp=gpar(fill="grey"))
grid.rect(width=.1, gp=gpar(fill="2"))
popViewport()
HersheyLabel("clipping path based on text and circle and rect
with even-odd rule
draw large grey rect and thin red rect
both clipped to text and space between circle and rect
(PDF will ONLY include text in clipping path", y=.85)

######################
## Check resource exhaustion
grid.newpage()
for (i in 1:65) {
    pushViewport(viewport(clip=circleGrob()))
    grid.rect(gp=gpar(fill="grey"))
    HersheyLabel(paste0("viewport ", i, " with clip path
result is grey circle"))
    upViewport()
}

## Text being clipped by path
grid.newpage()
grid.text("testing", gp=gpar(col="grey", cex=3))
path <- circleGrob(r=.05, gp=gpar(fill=NA))
grid.draw(path)
pushViewport(viewport(clip=path))
grid.text("testing", gp=gpar(cex=3))
popViewport()
HersheyLabel("text clipped by circle", y=.8)
    
## Raster being clipped by path
grid.newpage()
grid.raster(matrix(c(.5, 1, 1, .5), nrow=2),
            width=.2, height=.2,
            interpolate=FALSE)
path <- circleGrob(r=.05, gp=gpar(fill=NA))
grid.draw(path)
pushViewport(viewport(clip=path))
grid.raster(matrix(c(0:1, 1:0), nrow=2),
            width=.2, height=.2,
            interpolate=FALSE)
popViewport()
HersheyLabel("raster clipped by circle", y=.8)




