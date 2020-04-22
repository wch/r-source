
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
## Gradients

## Simple linear gradient on grob
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
HersheyLabel("default linear gradient")

## Test linearGradient() arguments
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient(c("red", "yellow", "red"),
                                      c(0, .5, 1),
                                      x1=.5, y1=unit(1, "in"), 
                                      x2=.5, y2=1,
                                      extend="none")))
HersheyLabel("linear gradient
1 inch from bottom
red-yellow-red")

## Gradient relative to grob
grid.newpage()
grid.rect(width=.5, height=.5,
          gp=gpar(fill=linearGradient()))
HersheyLabel("gradient on rect
rect half height/width")

## Gradient on viewport
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("default linear gradient
on viewport")

## Gradient relative to viewport
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect(width=.5, height=.5)
HersheyLabel("linear gradient on viewport
viewport whole page
rect half height/width")

grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("linear gradient on viewport
viewport half height/width
rect whole viewport")

## Inherited gradient on viewport
## (should be relative to first, larger viewport)
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
pushViewport(viewport(width=.5, height=.5))
grid.rect()
HersheyLabel("gradient on viewport
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
HersheyLabel("gradient on viewport
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
HersheyLabel("gradient from white to transparent
over text", y=.1)

## Radial gradient
grid.newpage()
grid.rect(gp=gpar(fill=radialGradient()))
HersheyLabel("default radial gradient")

## Test radialGradient() arguments
grid.newpage()
grid.rect(gp=gpar(fill=radialGradient(c("white", "black"),
                                      cx1=.8, cy1=.8)))    
HersheyLabel("radial gradient
white to black
start centre top-right")

## Gradient on a gTree
grid.newpage()
grid.draw(gTree(children=gList(rectGrob(gp=gpar(fill=linearGradient())))))
HersheyLabel("gTree with rect child
gradient on rect")

grid.newpage()
grid.draw(gTree(children=gList(rectGrob()), gp=gpar(fill=linearGradient())))
HersheyLabel("gTree with rect child
gradient on gTree")

## Rotated gradient
grid.newpage()
pushViewport(viewport(width=.5, height=.5, angle=45,
                      gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("rotated gradient")

######################################
## Tests of replaying graphics engine display list

## Resize graphics device
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
HersheyLabel("default gradient
(for resizing)")

grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("gradient on viewport
(for resizing)")

## Copy to new graphics device
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
x <- recordPlot()
HersheyLabel("default gradient
for recordPlot()")
replayPlot(x)
HersheyLabel("default gradient
from replayPlot()")
## (Resize that as well if you like)

grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
x <- recordPlot()
HersheyLabel("gradient on viewport
for recordPlot()")
replayPlot(x)
HersheyLabel("gradient on viewport
from replayPlot()")

## Replay on new device with gradient already defined
## (watch out for recorded grob using existing gradient)
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
x <- recordPlot()
HersheyLabel("default gradient
for recordPlot()")
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient(c("white", "red"))))
replayPlot(x)
HersheyLabel("default gradient
from replayPlot()
AFTER white-red gradient
(should be default gradient)")

## Similar to previous, except involving viewports
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
x <- recordPlot()
HersheyLabel("gradient on viewport
for recordPlot()")
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient(c("white", "red")))))
grid.rect()
replayPlot(x)
HersheyLabel("gradient on viewport
from replayPlot()
AFTER white-red gradient
(should be default gradient)")

######################################
## Test of 'grid' display list

grid.newpage()
grid.rect(name="r")
HersheyLabel("empty rect")
grid.edit("r", gp=gpar(fill=linearGradient()))
HersheyLabel("edited rect
to add gradient", y=.1)

grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
x <- grid.grab()
HersheyLabel("default gradient
for grid.grab()")
grid.newpage()
grid.draw(x)
HersheyLabel("default gradient
from grid.grab()")

grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()
x <- grid.grab()
HersheyLabel("gradient on viewport
viewport half height/width
for grid.grab")
grid.newpage()
grid.draw(x)
HersheyLabel("gradient on viewport
viewport half height/width
from grid.grab")

######################################
## Tests of "efficiency"
## (are patterns being resolved only as necessary)

## 
trace(grid:::resolveFill.GridPattern, print=FALSE,
      function(...) cat("*** RESOLVE:  Viewport pattern resolved\n"))
trace(grid:::resolveFill.GridGrobPattern, print=FALSE,
      function(...) cat("*** RESOLVE:  Grob pattern resolved\n"))

## ONCE for rect grob
cat("*** RESOLVE:  ONE resolve for rect grob with gradient\n")
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
HersheyLabel("default gradient
for tracing")

## ONCE for multiple rects from single grob
cat("*** RESOLVE:  ONE resolve for multiple rects from rect grob with gradient\n")
grid.newpage()
grid.rect(x=1:5/6, y=1:5/6, width=1/8, height=1/8,
          gp=gpar(fill=linearGradient()))
HersheyLabel("gradient on five rects
for tracing")

## ONCE for viewport with rect
cat("*** RESOLVE:  ONE resolve for rect grob in viewport with gradient\n")
grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("gradient on viewport
viewport half height/width
for tracing")

## ONCE for viewport with rect, revisiting multiple times
cat("*** RESOLVE:  ONE resolve for rect grob in viewport with gradient, plus nested viewport, plus viewport revisited\n")
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
HersheyLabel("gradient on viewport
viewport half width/height
rect (thick black border)
nested viewport (inherits gradient)
rect (medium red border)
navigate to original viewport
rect (thin blue border)")

untrace(grid:::resolveFill.GridPattern)
untrace(grid:::resolveFill.GridGrobPattern)

######################################
## Test for running out of patterns

## Should NOT run out of patterns
grid.newpage()
for (i in 1:21) {
    grid.rect(gp=gpar(fill=linearGradient()))
    HersheyLabel(paste0("rect ", i, " with gradient
new pattern every time"))
}

## Should run out of patterns
grid.newpage()
for (i in 1:21) {
    result <- try(pushViewport(viewport(gp=gpar(fill=linearGradient()))))
    if (!inherits(result, "try-error")) {
        grid.rect()
        HersheyLabel(paste0("viewport ", i, " with gradient
runs out after 20"))
    }
}

## grid.newpage() should fix it
grid.newpage()
for (i in 1:21) {
    grid.rect(gp=gpar(fill=linearGradient()))
    HersheyLabel(paste0("rect ", i, " with gradient
AFTER grid.newpage()
new pattern every time"))
}

################################################################################
## Grob-based patterns

## Simple circle grob as pattern in rect
grid.newpage()
grid.rect(gp=gpar(fill=pattern(circleGrob(gp=gpar(fill="grey")))))
HersheyLabel("single grey filled circle pattern")

## Multiple circles as pattern in rect
grid.newpage()
pat <- circleGrob(1:3/4, r=unit(1, "cm"))
grid.rect(gp=gpar(fill=pattern(pat)))
HersheyLabel("three unfilled circles pattern")

## Pattern on rect scales with rect
grid.newpage()
grid.rect(width=.5, height=.8, gp=gpar(fill=pattern(pat)))
HersheyLabel("pattern on rect scales with rect")

## Pattern on viewport
grid.newpage()
pushViewport(viewport(gp=gpar(fill=pattern(pat))))
grid.rect()
HersheyLabel("pattern on viewport
applied to rect")

## Pattern on viewport stays fixed for rect
grid.newpage()
pushViewport(viewport(gp=gpar(fill=pattern(pat))))
grid.rect(width=.5, height=.8)
HersheyLabel("pattern on viewport
applied to rect
pattern does not scale with rect")

## Patterns have colour
grid.newpage()
pat <- circleGrob(1:3/4, r=unit(1, "cm"),
                  gp=gpar(fill=c("red", "green", "blue")))
grid.rect(gp=gpar(fill=pattern(pat)))
HersheyLabel("pattern with colour")

## Pattern with gradient
grid.newpage()
pat <- circleGrob(1:3/4, r=unit(1, "cm"),
                  gp=gpar(fill=linearGradient()))
grid.rect(gp=gpar(fill=pattern(pat)))
HersheyLabel("pattern with gradient")

## Pattern with a clipping path
grid.newpage()
pat <- circleGrob(1:3/4, r=unit(1, "cm"),
                  vp=viewport(clip=rectGrob(height=unit(1, "cm"))),
                  gp=gpar(fill=linearGradient()))
grid.rect(gp=gpar(fill=pattern(pat)))
HersheyLabel("pattern with clipping path
and gradient")

## Pattern with makeContent() method
grid.newpage()
pat <- grob(cl="polkadots", gp=gpar(col=NA, fill="grey"))
makeContent.polkadots <- function(x) {
    w <- convertWidth(unit(1, "npc"), "cm", valueOnly=TRUE)
    h <- convertHeight(unit(1, "npc"), "cm", valueOnly=TRUE)
    x <- seq(0, ceiling(w))
    y <- seq(0, ceiling(h))
    xy <- expand.grid(x, y)
    circleGrob(xy$Var1, xy$Var2, r=unit(2, "mm"), default.units="cm")
}
grid.rect(gp=gpar(fill=pattern(pat)))
HersheyLabel("pattern that fills viewport")

grid.newpage()
pushViewport(viewport(gp=gpar(fill=pattern(pat))))
grid.rect(width=.5)
HersheyLabel("pattern that fills viewport
but only drawn within rectangle
pattern relative to viewport")

grid.newpage()
grid.rect(width=.5, gp=gpar(fill=pattern(pat)))
HersheyLabel("pattern as big as the viewport
but only drawn within rectangle
pattern relative to rectangle")


