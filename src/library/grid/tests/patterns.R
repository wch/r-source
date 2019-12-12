
library(grid)

################################################################################
## Gradients

## Simple linear gradient on grob
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
grid.text("default linear gradient")

## Test linearGradient() arguments
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient(c("red", "yellow", "red"),
                                      c(0, .5, 1),
                                      x1=.5, y1=unit(1, "in"), 
                                      x2=.5, y2=1,
                                      extend="none")))
grid.text("linear gradient
1 inch from bottom
red-yellow-red")

## Gradient relative to grob
grid.newpage()
grid.rect(width=.5, height=.5,
          gp=gpar(fill=linearGradient()))
grid.text("gradient on rect
rect half height/width")

## Gradient on viewport
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
grid.text("default linear gradient
on viewport")

## Gradient relative to viewport
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect(width=.5, height=.5)
grid.text("linear gradient on viewport
viewport whole page
rect half height/width")

grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()
grid.text("linear gradient on viewport
viewport half height/width
rect whole viewport")

## Inherited gradient on viewport
## (should be relative to first, larger viewport)
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
pushViewport(viewport(width=.5, height=.5))
grid.rect()
grid.text("gradient on viewport
viewport whole page
nested viewport half height/width
rect whole viewport")

## Restore of gradient (just like any other gpar)
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect(x=.2, width=.2, height=.5)
pushViewport(viewport(gp=gpar(fill="green")))
grid.rect(x=.5, width=.2, height=.5)
popViewport()
grid.rect(x=.8, width=.2, height=.5)
grid.text("gradient on viewport
viewport whole page
rect left third (gradient)
nested viewport whole page
nested viewport green fill
rect centre (green)
pop to first viewport
rect right third (gradient)")

## Translucent gradient
grid.newpage()
grid.text("Reveal", gp=gpar(fontface="bold", cex=3))
grid.rect(gp=gpar(fill=linearGradient(c("white", "transparent"),
                                      y1=.5, y2=.5)))
grid.text("gradient from white to transparent
over text", y=.1)

## Radial gradient
grid.newpage()
grid.rect(gp=gpar(fill=radialGradient()))
grid.text("default radial gradient")

## Test radialGradient() arguments
grid.newpage()
grid.rect(gp=gpar(fill=radialGradient(c("white", "black"),
                                      cx1=.8, cy1=.8)))    
grid.text("radial gradient
white to black
start centre top-right")

## Gradient on a gTree
grid.newpage()
grid.draw(gTree(children=gList(rectGrob(gp=gpar(fill=linearGradient())))))
grid.text("gTree with rect child
gradient on rect")

grid.newpage()
grid.draw(gTree(children=gList(rectGrob()), gp=gpar(fill=linearGradient())))
grid.text("gTree with rect child
gradient on gTree")

## Rotated gradient
grid.newpage()
pushViewport(viewport(width=.5, height=.5, angle=45,
                      gp=gpar(fill=linearGradient())))
grid.rect()
grid.text("rotated gradient")

######################################
## Tests of replaying graphics engine display list

## Resize graphics device
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
grid.text("default gradient
(for resizing)")

grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
grid.text("gradient on viewport
(for resizing)")

## Copy to new graphics device
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
x <- recordPlot()
grid.text("default gradient
for recordPlot()")
grid.newpage()
x
grid.text("default gradient
from replayPlot()")
## (Resize that as well if you like)

grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
x <- recordPlot()
grid.text("gradient on viewport
for recordPlot()")
grid.newpage()
x
grid.text("gradient on viewport
from replayPlot()")

## Replay on new device with gradient already defined
## (watch out for recorded grob using existing gradient)
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
x <- recordPlot()
grid.text("default gradient
for recordPlot()")
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient(c("white", "red"))))
x
grid.text("default gradient
from replayPlot()
AFTER white-red gradient
(should be default gradient)")

## Similar to previous, except involving viewports
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
grid.text("gradient on viewport
for recordPlot()")
x <- recordPlot()
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient(c("white", "red")))))
grid.rect()
x
grid.text("gradient on viewport
from replayPlot()
AFTER white-red gradient
(should be default gradient)")

######################################
## Test of 'grid' display list

grid.newpage()
grid.rect(name="r")
grid.text("empty rect")
grid.edit("r", gp=gpar(fill=linearGradient()))
grid.text("edited rect
to add gradient", y=.1)

grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
x <- grid.grab()
grid.text("default gradient
for grid.grab()")
grid.newpage()
grid.draw(x)
grid.text("default gradient
from grid.grab()")

grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()
x <- grid.grab()
grid.text("gradient on viewport
viewport half height/width
for grid.grab")
grid.newpage()
grid.draw(x)
grid.text("gradient on viewport
viewport half height/width
from grid.grab")

######################################
## Tests of "efficiency"
## (are patterns being resolved only as necessary)

## 
trace(grid:::resolveFill.GridPattern, print=FALSE,
      function(...) cat("Viewport pattern resolved\n"))
trace(grid:::resolveFill.GridGrobPattern, print=FALSE,
      function(...) cat("Grob pattern resolved\n"))

## ONCE for rect grob
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
grid.text("default gradient
for tracing")

## ONCE for multiple rects from single grob
grid.newpage()
grid.rect(x=1:5/6, y=1:5/6, width=1/8, height=1/8,
          gp=gpar(fill=linearGradient()))
grid.text("gradient on five rects
for tracing")

## ONCE for viewport with rect
grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()
grid.text("gradient on viewport
viewport half height/width
for tracing")

## ONCE for viewport with rect, revisiting multiple times
grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient()),
                      name="vp"))
grid.rect(gp=gpar(lwd=8))
pushViewport(viewport(width=.5, height=.5))
grid.rect()
upViewport()
grid.rect(gp=gpar(col="red", lwd=4))
upViewport()
downViewport("vp")
grid.rect(gp=gpar(col="blue", lwd=2))
grid.text("gradient on viewport
viewport half width/height
rect (thick black border)
nested viewport (inherits gradient)
rect (medium red border)
navigate to original viewport
rect (thin blue border)")

######################################
## Test for running out of patterns

## Should NOT run out of patterns
grid.newpage()
for (i in 1:21) {
    grid.rect(gp=gpar(fill=linearGradient()))
    grid.text(paste0("rect ", i, " with gradient
new pattern every time"))
}

## Should run out of patterns
grid.newpage()
for (i in 1:21) {
    try(pushViewport(viewport(gp=gpar(fill=linearGradient()))))
    grid.text(paste0("viewport ", i, " with gradient
runs out after 20"))
}

## grid.newpage() should fix it
grid.newpage()
for (i in 1:21) {
    grid.rect(gp=gpar(fill=linearGradient()))
    grid.text(paste0("rect ", i, " with gradient
AFTER grid.newpage()
new pattern every time"))
}


