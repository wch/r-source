
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
HersheyLabel("default linear gradient
black bottom-left to white top-right")

## Test linearGradient() arguments
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient(c("red", "yellow", "red"),
                                      c(0, .5, 1),
                                      x1=.5, y1=unit(1, "in"), 
                                      x2=.5, y2=1,
                                      extend="none")))
HersheyLabel("vertical linear gradient
1 inch from bottom
red-yellow-red")

## Gradient relative to grob
grid.newpage()
grid.rect(width=.5, height=.5,
          gp=gpar(fill=linearGradient()))
HersheyLabel("gradient on rect
black bottom-left to white top-right OF RECT")

## Gradient on viewport
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("default linear gradient on viewport
black bottom-left to white top-right")

## Gradient relative to viewport
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect(width=.5, height=.5)
HersheyLabel("linear gradient on viewport
viewport whole page
rect half height/width
darker grey (not black) bottom-left OF RECT
lighter grey (not white) top-right OF RECT")

grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("linear gradient on viewport
viewport half height/width
rect whole viewport
black bottom-left to white top-right OF RECT")

## Inherited gradient on viewport
## (should be relative to first, larger viewport)
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
pushViewport(viewport(width=.5, height=.5))
grid.rect()
HersheyLabel("gradient on viewport
viewport whole page
nested viewport half height/width
rect whole viewport
darker grey (not black) bottom-left OF RECT
lighter grey (not white) top-right OF RECT")

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
rect left third (gradient from whole page)
nested viewport whole page
nested viewport green fill
rect centre (green)
pop to first viewport
rect right third (gradient from whole page)")

## Translucent gradient
grid.newpage()
grid.text("Reveal", gp=gpar(fontfamily="HersheySans",
                            fontface="bold", cex=3))
grid.rect(gp=gpar(fill=linearGradient(c("white", "transparent"),
                                      x1=.4, x2=.6, y1=.5, y2=.5)))
HersheyLabel("gradient from white to transparent
over text", y=.1)

## Radial gradient
grid.newpage()
grid.rect(gp=gpar(fill=radialGradient()))
HersheyLabel("default radial gradient
black centre to white radius", y=.1)

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
gradient on rect
black bottom-left to white top-right")

grid.newpage()
grid.draw(gTree(children=gList(rectGrob()), gp=gpar(fill=linearGradient())))
HersheyLabel("gTree with rect child
gradient on gTree
black bottom-left to white top-right")

## Rotated gradient
grid.newpage()
pushViewport(viewport(width=.5, height=.5, angle=45,
                      gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("rotated gradient
black bottom-left to white top-right OF RECT")

######################################
## Tests of replaying graphics engine display list

## Resize graphics device
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
HersheyLabel("default gradient
(for resizing)
black bottom-left to white top-right")

grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("gradient on viewport
(for resizing)
black bottom-left to white top-right")

## Copy to new graphics device
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
x <- recordPlot()
HersheyLabel("default gradient
for recordPlot()
black bottom-left to white top-right")
replayPlot(x)
HersheyLabel("default gradient
from replayPlot()
black bottom-left to white top-right")
## (Resize that as well if you like)

grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
x <- recordPlot()
HersheyLabel("gradient on viewport
for recordPlot()
black bottom-left to white top-right")
replayPlot(x)
HersheyLabel("gradient on viewport
from replayPlot()
black bottom-left to white top-right")

## Replay on new device with gradient already defined
## (watch out for recorded grob using existing gradient)
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
x <- recordPlot()
HersheyLabel("default gradient
for recordPlot()
black bottom-left to white top-right")
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient(c("white", "red"))))
HersheyLabel("new rect with new gradient")
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
HersheyLabel("new viewport with new gradient")
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
HersheyLabel("rect with gradient
(for grab)")
x <- grid.grab()
grid.newpage()
grid.draw(x)
HersheyLabel("default gradient
from grid.grab()")

grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()
HersheyLabel("gradient on viewport
viewport half height/width
for grid.grab")
x <- grid.grab()
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
traceHead <- "ONE resolve for rect grob with gradient"
grid.newpage()
traceOutput <- capture.output(grid.rect(gp=gpar(fill=linearGradient())))
HersheyLabel("default gradient
for tracing", y=.9)
HersheyLabel(paste(traceHead, paste(traceOutput, collapse="\n"), sep="\n"))


## ONCE for multiple rects from single grob
traceHead <- "ONE resolve for multiple rects from rect grob with gradient"
grid.newpage()
traceOutput <- capture.output(grid.rect(x=1:5/6, y=1:5/6, width=1/8, height=1/8,
                                        gp=gpar(fill=linearGradient())))
HersheyLabel("gradient on five rects
for tracing", y=.9)
HersheyLabel(paste(traceHead, paste(traceOutput, collapse="\n"), sep="\n"))

## ONCE for viewport with rect
traceHead <- "ONE resolve for rect grob in viewport with gradient"
grid.newpage()
traceOutput <- capture.output({
    pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
    grid.rect()
})
HersheyLabel("gradient on viewport
viewport half height/width
for tracing", y=.8)
HersheyLabel(paste(traceHead, paste(traceOutput, collapse="\n"), sep="\n"))

## ONCE for viewport with rect, revisiting multiple times
traceHead <- "ONE resolve for rect grob in viewport with gradient\nplus nested viewport\nplus viewport revisited"
grid.newpage()
traceOutput <- capture.output({
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
})
HersheyLabel("gradient on viewport
viewport half width/height
rect (thick black border)
nested viewport (inherits gradient)
rect (medium red border)
navigate to original viewport
rect (thin blue border)", y=.9)
HersheyLabel(paste(traceHead, paste(traceOutput, collapse="\n"), sep="\n"))

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
    pushViewport(viewport(gp=gpar(fill=linearGradient())))
    grid.rect()
    HersheyLabel(paste0("viewport ", i, " with gradient
runs out after 20"))
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

## Tiling patterns
grid.newpage()
grob <- circleGrob(r=unit(2, "mm"),
                   gp=gpar(col=NA, fill="grey"))
pat <- pattern(grob,
               width=unit(5, "mm"),
               height=unit(5, "mm"),
               extend="repeat")
grid.rect(gp=gpar(fill=pat))
HersheyLabel("pattern that tiles page")

grid.newpage()
pushViewport(viewport(gp=gpar(fill=pat)))
grid.rect(width=.5)
HersheyLabel("pattern that fills viewport
but only drawn within rectangle
pattern relative to viewport")

grid.newpage()
grob <- circleGrob(x=0, y=0, r=unit(2, "mm"),
                   gp=gpar(col=NA, fill="grey"))
pat <- pattern(grob,
               x=0, y=0, 
               width=unit(5, "mm"),
               height=unit(5, "mm"),
               extend="repeat")
grid.rect(width=.5, gp=gpar(fill=pat))
HersheyLabel("pattern as big as the viewport
but only drawn within rectangle
pattern relative to rectangle
(starts at bottom left of rectangle)")

## More tests
grid.newpage()
grid.circle(gp=gpar(fill=linearGradient(y1=.5, y2=.5)))
HersheyLabel("circle with horizontal gradient
black left to white right")

grid.newpage()
grid.polygon(c(.2, .8, .7, .5, .3),
             c(.8, .8, .2, .4, .2),
             gp=gpar(fill=linearGradient(y1=.5, y2=.5)))
HersheyLabel("polygon with horizontal gradient
black left to white right")

grid.newpage()
grid.path(c(.2, .8, .3, .5, .7),
          c(.8, .8, .2, .4, .2),
          gp=gpar(fill=linearGradient(y1=.5, y2=.5)))
HersheyLabel("path with horizontal gradient
black left to white right")

grid.newpage()
grid.text("Reveal", gp=gpar(fontfamily="HersheySans",
                            fontface="bold", cex=3))
grid.rect(gp=gpar(col=NA,
                  fill=radialGradient(c("white", "transparent"),
                                      r2=.3)))
HersheyLabel("text with semitransparent radial gradient
centre of text should be dissolved", y=.2)

grid.newpage()
pat <-
    pattern(circleGrob(gp=gpar(col=NA, fill="grey"),
                       vp=viewport(width=.2, height=.2,
                                   mask=rectGrob(x=c(1, 3)/4,
                                                 width=.3,
                                                 gp=gpar(fill="black")))),
            width=1/4, height=1/4,
            extend="repeat")
grid.rect(width=.5, height=.5, gp=gpar(fill=pat))
HersheyLabel("rect in centre with pattern fill
pattern is circle drawn in smaller viewport
pattern is masked by two tall thin rects
pattern repeats", y=.15)

grid.newpage()
pat1 <-
    pattern(circleGrob(r=.1, gp=gpar(col="black", fill="grey")),
            width=.2, height=.2,
            extend="repeat")
pat2 <-
    pattern(circleGrob(r=1/4, gp=gpar(col="black", fill=pat1)),
            width=1/2, height=1/2,
            extend="repeat")
grid.rect(width=.5, height=.5, gp=gpar(fill=pat2))
HersheyLabel("rect in centre with pattern fill
pattern is small circle with pattern fill
nested pattern is smaller circle (grey)
both patterns repeat", y=.15)

