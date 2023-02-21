
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
                                   mask=devMask(rectGrob(x=c(1, 3)/4,
                                                         width=.3,
                                                         gp=gpar(fill="black")),
                                                rectGrob(x=c(1, 3)/4,
                                                         width=.3,
                                                         gp=gpar(col="white",
                                                                 fill="white"))))),
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

######################################
## Test for expanding pattern resources
grid.newpage()
for (i in 1:21) {
    grid.rect(gp=gpar(fill=linearGradient()))
    HersheyLabel(paste0("rect ", i, " with gradient
pattern released every time"))
}

grid.newpage()
for (i in 1:65) {
    pushViewport(viewport(gp=gpar(fill=linearGradient())))
    grid.rect()
    HersheyLabel(paste0("viewport ", i, " with gradient
new pattern every time"))
}

grid.newpage()
for (i in 1:21) {
    grid.rect(gp=gpar(fill=linearGradient()))
    HersheyLabel(paste0("rect ", i, " with gradient
AFTER grid.newpage()
pattern released every time"))
}

####################################
## Additional tests

## gTree with gradient fill
grid.newpage()
gt <- gTree(children=gList(circleGrob(1:2/3, r=.1)),
            gp=gpar(fill=linearGradient(y1=.5, y2=.5)))
grid.draw(gt)
HersheyLabel("gTree with circles as children
gTree has gradient fill
gradient relative to circle bounds
(black at left to white at right)", y=.8)

## gTree with gradient fill with gTree 
grid.newpage()
gt <- gTree(children=gList(gTree(children=gList(circleGrob(1:2/3, r=.1)))),
            gp=gpar(fill=linearGradient(y1=.5, y2=.5)))
grid.draw(gt)
HersheyLabel("gTree with gTree as child
inner gTree has circles as children
outer gTree has gradient fill
gradient relative to circle bounds
(black at left to white at right)", y=.8)

## Pattern including text
grid.newpage()
pat <- pattern(textGrob("test"),
               width=1.2*stringWidth("test"),
               height=unit(1, "lines"),
               extend="repeat")
grid.circle(r=.3, gp=gpar(fill=pat))
HersheyLabel("circle filled with pattern
pattern based on (repeating) text", y=.9)

## Text (path) filled with pattern
grid.newpage()
rects <- gTree(children=gList(rectGrob(width=unit(2, "mm"),
                                       height=unit(2, "mm"),
                                       just=c("left", "bottom"),
                                       gp=gpar(fill="black")),
                              rectGrob(width=unit(2, "mm"),
                                       height=unit(2, "mm"),
                                       just=c("right", "top"),
                                       gp=gpar(fill="black"))))
checkerBoard <- pattern(rects,
                        width=unit(4, "mm"), height=unit(4, "mm"),
                        extend="repeat")
grid.fill(textGrob("test", gp=gpar(fontface="bold", cex=10)),
          gp=gpar(fill=checkerBoard))
HersheyLabel("stroked path based on text
filled with checkerboard pattern", y=.8)
    
## Pattern including raster
grid.newpage()
rg <- rasterGrob(matrix(c(0:1, 1:0), nrow=2),
                 width=unit(1, "cm"), height=unit(1, "cm"),
                 interpolate=FALSE)
pat <- pattern(rg, 
               width=unit(1, "cm"), height=unit(1, "cm"),
               extend="repeat")
grid.circle(r=.2, gp=gpar(fill=pat))
HersheyLabel("circle filled with pattern
pattern is based on raster (checkerboard)", y=.8)

## Radial gradient where start circle and final circle overlap
grid.newpage()
x1 <- .7
y1 <- .7
r1 <- .2
x2 <- .4
y2 <- .4
r2 <- .4
grid.circle(x1, y1, r=r1, gp=gpar(col="green", fill=NA, lwd=2))
grid.circle(x2, y2, r=r2, gp=gpar(col="red", fill=NA, lwd=2))
grid.rect(gp=gpar(fill=radialGradient(rgb(0:1, 1:0, 0, .5),
                                      cx1=x1, cy1=y1, r1=r1,
                                      cx2=x2, cy2=y2, r2=r2)))
HersheyLabel("radial gradient with overlapping start and final circles
gradient is from semitransparent green
to semitransparent red
start circle is green
final circle is red")

## Text (path) filled with pattern
grid.newpage()
grid.fill(textGrob("test", gp=gpar(fontface="bold", cex=10)),
          gp=gpar(fill=linearGradient(2:3)))
HersheyLabel("stroked path based on text
filled with linear gradient", y=.8)
    
################################################################################
## Points

## Points filled with gradient
grid.newpage()
grid.points(1:9/10, 1:9/10, default.units="npc",
            pch=21, gp=gpar(fill=linearGradient()))
HersheyLabel("points (pch=21)
filled with linear gradient
(gradient based on ALL points)", y=.8)

## Points filled with gradient (point not filled)
grid.newpage()
grid.points(1:9/10, 1:9/10, default.units="npc",
            pch=1, gp=gpar(fill=linearGradient()))
HersheyLabel("points (pch=1)
filled with linear gradient
(fill ignored)", y=.8)

## Individual points filled with gradient (gradient recycled)
grid.newpage()
grid.points(1:3/4, 1:3/4, default.units="npc",
            pch=21, gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("points (pch=21)
filled with linear gradient
(gradient based on EACH point)", y=.8)

## Individual points filled with individual gradients
grid.newpage()
gradients <- lapply(2:4, function(x) linearGradient(c(x, "white"), group=FALSE))
grid.points(1:3/4, 1:3/4, default.units="npc",
            pch=21, gp=gpar(fill=gradients))
HersheyLabel("points (pch=21)
filled with linear gradient
(different gradient for EACH point)", y=.8)

## points inheriting single gradient
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.points(1:2, 1:2, default.units="in", pch=21)
HersheyLabel("points (pch=21)
filled with linear gradient
gradient inherited from viewport
(so gradient relative to viewport)")

## points inheriting multiple gradients
grid.newpage()
pushViewport(viewport(gp=gpar(fill=list(linearGradient(1:2),
                                        radialGradient(3:4)))))
grid.points(1:2, 1:2, default.units="in", pch=21)
HersheyLabel("points (pch=21)
filled with multiple linear gradients
gradients inherited from viewport
(so gradients relative to viewport)")

## points recycling inherited multiple gradients
grid.newpage()
pushViewport(viewport(gp=gpar(fill=list(linearGradient(1:2),
                                        radialGradient(3:4)))))
grid.points(1:9/10, 1:9/10, default.units="npc", pch=21)
HersheyLabel("points (pch=21)
filled with linear gradients
gradients inherited from viewport
(so gradient relative to viewport)
more points than gradients
(so gradients recycled)")

## points recycling inherited multiple gradients with group=FALSE
## so pattern just passed through and resolved relative to points grob
grid.newpage()
pushViewport(viewport(gp=gpar(fill=list(linearGradient(1:2, group=FALSE),
                                        radialGradient(3:4, group=FALSE)))))
grid.points(1:9/10, 1:9/10, default.units="npc", pch=21)
HersheyLabel("points (pch=21)
filled with linear gradients
group=FALSE
gradients inherited from viewport
(but unresolved so resolved on EACH point)
more points than gradients
(so gradients recycled)")

## Using tracing to check that fills are not being resolved more than necessary
trace(grid:::resolveFill.GridPattern, print=FALSE,
      function(...) cat("*** RESOLVE:  Viewport pattern resolved\n"))
trace(grid:::resolveFill.GridPatternList, print=FALSE,
      function(...) cat("*** RESOLVE:  Viewport pattern list resolved\n"))
trace(grid:::resolveFill.GridGrobPattern, print=FALSE,
      function(...) cat("*** RESOLVE:  Grob pattern resolved\n"))
trace(grid:::resolveFill.GridGrobPatternList, print=FALSE,
      function(...) cat("*** RESOLVE:  Grob pattern list resolved\n"))
doTrace <- function(head, f) {
    traceOutput <- capture.output(f())    
    HersheyLabel(paste(head, paste(traceOutput, collapse="\n"), sep="\n"))
}

grid.newpage()
doTrace("points grob (pch=21)\nwith gradient\nONE resolve",
        function() {
            grid.points(1:9/10, 1:9/10, default.units="npc",
                        pch=21,
                        gp=gpar(fill=linearGradient()))
        })

grid.newpage()
doTrace("points grob (pch=1)\nwith gradient\nONE resolve\n(even though unused)",
        function() {
            grid.points(1:9/10, 1:9/10, default.units="npc",
                        pch=1, gp=gpar(fill=linearGradient()))
        })

grid.newpage()
doTrace("points grob (pch=21)\nwith gradient (group=FALSE)\nTHREE resolves\n(resolve per point)",
        function() {
            grid.points(1:3/4, 1:3/4, default.units="npc",
                        pch=21, gp=gpar(fill=linearGradient(group=FALSE)))
        })

grid.newpage()
gradients <- lapply(2:4, function(x) linearGradient(c(x, "white"), group=FALSE))
doTrace("points grob (pch=21)\nwith gradient list (group=FALSE)\nONE resolve\n(all gradients resolved at once)",
        function() {
            grid.points(1:3/4, 1:3/4, default.units="npc",
                        pch=21, gp=gpar(fill=gradients))
        })

grid.newpage()
doTrace("points grob (pch=21)\nwith inherited gradient\nONE resolve\n(gradient resolved when vp pushed)",
        function() {
            pushViewport(viewport(gp=gpar(fill=linearGradient())))
            grid.points(1:2, 1:2, default.units="in", pch=21)
        })

grid.newpage()
doTrace("points grob (pch=21)\nwith inherited gradient list\nTWO resolves\n(gradient list resolved when vp pushed\nAND gradient list resolved when points drawn\n[no-op because already resolved])",
        function() {
            pushViewport(viewport(gp=gpar(fill=list(linearGradient(1:2),
                                                    radialGradient(3:4)))))
            grid.points(1:2, 1:2, default.units="in", pch=21)
        })

grid.newpage()
doTrace("points grob (pch=21)\nwith inherited gradient list\nAND recycling of gradients\nTWO resolves\n(gradient list resolved when vp pushed\nAND gradient list resolved when points drawn\n[no-op because already resolved])",
        function() {
            pushViewport(viewport(gp=gpar(fill=list(linearGradient(1:2),
                                                    radialGradient(3:4)))))
            grid.points(1:9/10, 1:9/10, default.units="npc", pch=21)
        })

## Individual points filled with individual gradients
## *some* group = TRUE and *some* group = FALSE
grid.newpage()
gradients <- lapply(2:4, function(x) linearGradient(c(x, "white"),
                                                    group = x %% 2))
grid.points(1:3/4, 1:3/4, default.units="npc",
            pch=21, gp=gpar(fill=gradients))
HersheyLabel("points (pch=21)
filled with linear gradient
(different gradient for EACH point)
first and third resolved on individual points
second resolved on ALL points", y=.8)

## Points filled with pattern (recycled), multiple pch
grid.newpage()
grid.points(1:3/4, 1:3/4, default.units="npc",
            pch=21:23, gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("points (pch=21:23)
single gradient (group=FALSE)
each different point gets its own gradient", y=.8)

################################################################################
## Rects
grid.newpage()
grid.rect(x=1:3/4, y=1:3/4, width=.2, height=.2,
          gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE", y=.8)

grid.newpage()
grid.rect(x=1:3/4, y=1:3/4, width=.2, height=.2,
          gp=gpar(fill=list(linearGradient(group=FALSE),
                            radialGradient(group=FALSE),
                            linearGradient())))
HersheyLabel("list of gradient fills
linear (group=FALSE)
radial (group=FALSE)
linear (group=TRUE)", y=.8)

################################################################################
## Circles
grid.newpage()
grid.circle(x=1:3/4, y=1:3/4, r=.1,
            gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE", y=.8)

grid.newpage()
grid.circle(x=1:3/4, y=1:3/4, r=.1,
            gp=gpar(fill=list(linearGradient(group=FALSE),
                              radialGradient(group=FALSE),
                              linearGradient())))
HersheyLabel("list of gradient fills
linear (group=FALSE)
radial (group=FALSE)
linear (group=TRUE)", y=.8)

################################################################################
## Polygons
grid.newpage()
grid.polygon(x=c(.2, .4, .3,
                 .4, .6, .5,
                 .6, .8, .7),
             y=c(.2, .2, .4,
                 .4, .4, .6,
                 .6, .6, .8),
             id=rep(1:3, each=3),
             gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE", y=.8)

grid.newpage()
grid.polygon(x=c(.2, .4, .3,
                 .4, .6, .5,
                 .6, .8, .7),
             y=c(.2, .2, .4,
                 .4, .4, .6,
                 .6, .6, .8),
             id=rep(1:3, each=3),
             gp=gpar(fill=list(linearGradient(group=FALSE),
                               radialGradient(group=FALSE),
                               linearGradient())))
HersheyLabel("list of gradient fills
linear (group=FALSE)
radial (group=FALSE)
linear (group=TRUE)", y=.8)

################################################################################
## Segments
grid.newpage()
grid.segments(x0=c(.2, .4, .6),
              y0=c(.2, .5, .8),
              x1=c(.4, .6, .8),
              y1=c(.2, .5, .8),
              gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE", y=.8)

grid.newpage()
grid.segments(x0=c(.2, .4, .6),
              y0=c(.2, .5, .8),
              x1=c(.4, .6, .8),
              y1=c(.2, .5, .8),
             gp=gpar(fill=list(linearGradient(group=FALSE),
                               radialGradient(group=FALSE),
                               linearGradient())))
HersheyLabel("list of gradient fills
linear (group=FALSE)
radial (group=FALSE)
linear (group=TRUE)", y=.8)

################################################################################
## Xsplines
grid.newpage()
grid.xspline(x=c(.2, .4, .3,
                 .4, .6, .5,
                 .6, .8, .7),
             y=c(.2, .2, .4,
                 .4, .4, .6,
                 .6, .6, .8),
             id=rep(1:3, each=3),
             shape=-1, open=FALSE,
             gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE", y=.8)

grid.newpage()
grid.xspline(x=c(.2, .4, .3,
                 .4, .6, .5,
                 .6, .8, .7),
             y=c(.2, .2, .4,
                 .4, .4, .6,
                 .6, .6, .8),
             id=rep(1:3, each=3),
             shape=-1, open=FALSE,
             gp=gpar(fill=list(linearGradient(group=FALSE),
                               radialGradient(group=FALSE),
                               linearGradient())))
HersheyLabel("list of gradient fills
linear (group=FALSE)
radial (group=FALSE)
linear (group=TRUE)", y=.8)

################################################################################
## Lines
##
## NOTE that polylines are handled by same underlying C code
grid.newpage()
grid.lines(x=c(.2, .4, .3),
           y=c(.2, .2, .4),
           gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE", y=.8)

grid.newpage()
grid.lines(x=c(.2, .4, .3),
           y=c(.2, .2, .4),
           gp=gpar(fill=list(linearGradient(group=FALSE),
                             radialGradient(group=FALSE),
                             linearGradient())))
HersheyLabel("list of gradient fills
linear (group=FALSE)
radial (group=FALSE)
linear (group=TRUE)", y=.8)

################################################################################
## MoveTo/LineTo
grid.newpage()
grid.move.to(x=.2, y=.2)
grid.line.to(x=.4, y=.4,
             gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE", y=.8)

grid.newpage()
grid.move.to(x=.2, y=.2)
grid.line.to(x=.4, y=.4,
             gp=gpar(fill=list(linearGradient(group=FALSE),
                               radialGradient(group=FALSE),
                               linearGradient())))
HersheyLabel("list of gradient fills
linear (group=FALSE)
radial (group=FALSE)
linear (group=TRUE)", y=.8)

################################################################################
## Paths

## Pattern fill on single path consisting of distinct shapes
grid.newpage()
grid.path(c(.2, .2, .4, .4, .6, .6, .8, .8),
          c(.2, .4, .4, .2, .6, .8, .8, .6),
          id=rep(1:2, each=4),
          gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE
single path", y=.8)

## Pattern fill on multiple paths, each consisting of distinct shapes
grid.newpage()
grid.path(c(.2, .2, .4, .4,
            .25, .25, .35, .35,
            .6, .6, .8, .8,
            .65, .65, .75, .75),
          c(.2, .4, .4, .2,
            .25, .35, .35, .25,
            .6, .8, .8, .6,
            .65, .75, .75, .65),
          rule="evenodd",
          id=rep(1:4, each=4),
          pathId=rep(1:2, each=8),
          gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE
multiple paths", y=.8)

## Same thing, list of patterns
grid.newpage()
grid.path(c(.2, .2, .4, .4,
            .25, .25, .35, .35,
            .6, .6, .8, .8,
            .65, .65, .75, .75),
          c(.2, .4, .4, .2,
            .25, .35, .35, .25,
            .6, .8, .8, .6,
            .65, .75, .75, .65),
          rule="evenodd",
          id=rep(1:4, each=4),
          pathId=rep(1:2, each=8),
          gp=gpar(fill=list(linearGradient(group=FALSE),
                            radialGradient(group=FALSE))))
HersheyLabel("mulitple gradient fills
group = FALSE
multiple paths", y=.8)

################################################################################
## Raster
grid.newpage()
grid.raster(matrix(1:4/5, ncol=2),
            interpolate=FALSE,
            width=.5, height=.5,
            gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE", y=.8)

grid.newpage()
grid.raster(matrix(1:4/5, ncol=2),
            interpolate=FALSE,
            width=.5, height=.5,
            gp=gpar(fill=list(linearGradient(group=FALSE),
                              radialGradient(group=FALSE),
                              linearGradient())))
HersheyLabel("list of gradient fills
linear (group=FALSE)
radial (group=FALSE)
linear (group=TRUE)", y=.8)

################################################################################
## Text
grid.newpage()
grid.text(letters[1:3], x=1:3/4, y=1:3/4, 
          gp=gpar(fontfamily="HersheySans",
                  fill=linearGradient(group=FALSE)))
HersheyLabel("single gradient fill
group = FALSE", y=.8)

grid.newpage()
grid.text(letters[1:3], x=1:3/4, y=1:3/4, 
          gp=gpar(fontfamily="HersheySans",
                  fill=list(linearGradient(group=FALSE),
                            radialGradient(group=FALSE),
                            linearGradient())))
HersheyLabel("list of gradient fills
linear (group=FALSE)
radial (group=FALSE)
linear (group=TRUE)", y=.8)

################################################################################
## Arrows
grid.newpage()
grid.segments(x0=c(.2, .4, .6),
              y0=c(.2, .5, .8),
              x1=c(.4, .6, .8),
              y1=c(.2, .5, .8),
              arrow=arrow(type="closed"),
              gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("Lines with (closed) arrows
gradient fill disallowed on arrow", y=.8)

grid.newpage()
grid.xspline(x=c(.2, .4, .3,
                 .4, .6, .5,
                 .6, .8, .7),
             y=c(.2, .2, .4,
                 .4, .4, .6,
                 .6, .6, .8),
             id=rep(1:3, each=3),
             shape=-1,
             arrow=arrow(type="closed"),
             gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("Lines with (closed) arrows
gradient fill disallowed on arrow", y=.8)

grid.newpage()
grid.lines(x=c(.2, .4, .3),
           y=c(.2, .2, .4),
           arrow=arrow(type="closed"),
           gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("Lines with (closed) arrows
gradient fill disallowed on arrow", y=.8)

grid.newpage()
grid.move.to(x=.2, y=.2)
grid.line.to(x=.4, y=.4,
             arrow=arrow(type="closed"),
             gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("Lines with (closed) arrows
gradient fill disallowed on arrow", y=.8)

################################################################################
## Test more complex coords from more complex grobs (gTrees)

################################################################################
## grobCoords() also used when resolving patterns to generate a bbox
## for temporary viewport (so the pattern is resolved relative to the
## grob bbox).  Hence ...
##
## grid/R/patterns.R
library(grid)

## Test gTree with pattern fill
## Children are distinct rectangles, pattern is resolved on gTree
## so relative to bbox around both rectangles
gt <- gTree(children=gList(rectGrob(1/3, width=.2, height=.2),
                           rectGrob(2/3, width=.2, height=.2)),
            gp=gpar(fill=linearGradient()))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with two rects
fill resolved on bbox of both rects", y=.8)

## Test gTree with pattern fill with children that push vp
## (to test that the resolution happens in the gTree context
##  NOT the child's vp context)
## Both rects should be filled with gradient that fills whole page
gt <- gTree(children=gList(rectGrob(),
                           rectGrob(vp=viewport(width=.5, height=.5))),
            gp=gpar(fill=linearGradient()))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with two rects
one rect has vp
fill resolved on gTree
both rects same fill")

## Test gTree with pattern fill with children with pattern fill
## Left rect gets its own gradient;  right rect gets gradient
## relative to both rects
gt <- gTree(children=gList(rectGrob(1/3, width=.2, height=.2,
                                    gp=gpar(fill=linearGradient())),
                           rectGrob(2/3, width=.2, height=.2)),
            gp=gpar(fill=linearGradient()))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with pattern fill
one rect also has pattern fill
one rect has gTree pattern fill
(resolved on both rects)
one rect has its own pattern fill", y=.8)

## Test gTree with pattern fill with gTree as child
## (same result as first gTree test)
gt <- gTree(children=gList(gTree(children=gList(rectGrob(1/3,
                                                         width=.2,
                                                         height=.2),
                                                rectGrob(2/3,
                                                         width=.2,
                                                         height=.2)))),
            gp=gpar(fill=linearGradient()))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with pattern fill
child is gTree with children
pattern resolved on parent gTree" ,y=.8)

## Test gTree with gTree with pattern fill as child
## (same result as first gTree test)
gt <- gTree(children=gList(gTree(children=gList(rectGrob(1/3,
                                                         width=.2,
                                                         height=.2),
                                                rectGrob(2/3,
                                                         width=.2,
                                                         height=.2)),
                                 gp=gpar(fill=linearGradient()))))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree child gTree
child gTree has pattern fill
pattern resolved on child gTree" ,y=.8)

## Test gTree with pattern fill with group = FALSE
## (so pattern fill is resolved separately on each child)
gt <- gTree(children=gList(rectGrob(1/3, width=.2, height=.2),
                           rectGrob(2/3, width=.2, height=.2)),
            gp=gpar(fill=linearGradient(group=FALSE)))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with pattern fill
with group=FALSE
pattern resolved on each child rect", y=.8)

################################################################################
## groups and (stroked and filled) paths generate gTrees to calculate
## grobCoords(), so they are affected.  Hence ...
##
## grid/R/group.R
## grid/R/path.R
library(grid)
r1 <- rectGrob(x=0, y=0, width=.5, height=.5, just=c("left", "bottom"))
r2 <- rectGrob(x=1, y=1, width=.75, height=.75, just=c("right", "top"),
               gp=gpar(fill="black"))

## Path with hole filled with pattern
grid.newpage()
grid.fill(gTree(children=gList(r1, r2)),
          rule="evenodd",
          gp=gpar(fill=linearGradient()))
HersheyLabel("path from two rects
pattern fill resolved on bbox of both rects", y=.8)

## Remove r2 from r1 with "group" and fill with gradient
## (bbox is from BOTH rects, hence whole page)
grid.newpage()
grid.group(r2, "dest.out", r1, gp=gpar(fill=linearGradient()))
HersheyLabel("group of two rects
big rect takes bite out of small rect
pattern fill resolved on bbox of both rects", y=.8)

## NOTE that setting 'gp' on group use has no effect on group
## (graphical parameter settings were fixed at group definition)
grid.newpage()
grid.define(r1, name="r1")
pushViewport(viewport(x=1, y=1))
grid.use("r1", gp=gpar(fill=linearGradient()))
upViewport()
HersheyLabel("group use with pattern fill
pattern IGNORED", y=.2)

## BUT if put the fill on the grob in the group it works ?
grid.newpage()
grid.define(editGrob(r1, gp=gpar(fill=linearGradient())), name="r1")
pushViewport(viewport(x=1, y=1))
grid.use("r1")
upViewport()
HersheyLabel("group use imposes transformation
rect within group has pattern fill
pattern resolved on rect on use", y=.2)
## ... even with scaling (as well as translation) transformation
grid.newpage()
grid.define(editGrob(r1, gp=gpar(fill=linearGradient())), name="r1")
pushViewport(viewport(x=1, y=1, width=.5, height=.5))
grid.use("r1")
upViewport()
HersheyLabel("group use imposes transformation AND scaling
rect within group has pattern fill
pattern resolved on rect on use", y=.2)

################################################################################
## Tests of gTree with LIST of patterns

## gTree with LIST of patterns, group = TRUE
## Test gTree with pattern fill with group = FALSE
## (so pattern fill is resolved separately on each child)
gt <- gTree(children=gList(rectGrob(1:2/3, 1/3, width=.2, height=.2),
                           rectGrob(1:2/3, 2/3, width=.2, height=.2)),
            gp=gpar(fill=list(linearGradient(), radialGradient())))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with LIST of pattern fills
with group=TRUE
patterns resolved on gTree
each SHAPE within each child gets different pattern", y=.8)

## gTree with LIST of patterns, group = FALSE
gt <- gTree(children=gList(rectGrob(1:2/3, 1/3, width=.2, height=.2),
                           rectGrob(1:2/3, 2/3, width=.2, height=.2)),
            gp=gpar(fill=list(linearGradient(group=FALSE),
                              radialGradient(group=FALSE))))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with LIST of pattern fills
with group=FALSE
patterns resolved on children
each SHAPE within each child RESOLVES different pattern", y=.8)

## gTree with LIST of patterns, group = mix of TRUE/FALSE
gt <- gTree(children=gList(rectGrob(1:2/3, 1/3, width=.2, height=.2),
                           rectGrob(1:2/3, 2/3, width=.2, height=.2)),
            gp=gpar(fill=list(linearGradient(group=TRUE),
                              radialGradient(group=FALSE))))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with LIST of pattern fills
with group=TRUE and FALSE
patterns resolved on gTree AND children
each SHAPE within each child gets OR resolves different pattern", y=.8)

## gTree with LIST of patterns, group = TRUE
## but NO children that have a fill!
gt <- gTree(children=gList(segmentsGrob(0, 0:1, 1, 1:0)),
            gp=gpar(fill=list(linearGradient(),
                              radialGradient())))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with LIST of pattern fills
with group=TRUE
BUT no children that have a fill
patterns resolved on gTree
no (pattern) fill", y=.8)

## gTree with LIST of patterns, group = FALSE
## but NO children that have a fill!
gt <- gTree(children=gList(segmentsGrob(0, 0:1, 1, 1:0)),
            gp=gpar(fill=list(linearGradient(group=FALSE),
                              radialGradient(group=FALSE))))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with LIST of pattern fills
with group=FALSE
BUT no children that have a fill
patterns resolved on children
no (pattern) fill", y=.8)

## gTree with LIST of patterns, group = mix of TRUE/FALSE
## and MIX of children that have a fill!
## (all combinations of group and child-has-fill)
gt <- gTree(children=gList(segmentsGrob(0, 0:1, 1, 1:0),
                           rectGrob(1:2/3, 2/3, width=.2, height=.2)),
            gp=gpar(fill=list(linearGradient(group=TRUE),
                              radialGradient(group=FALSE))))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with LIST of pattern fills
with group=FALSE
BUT no children that have a fill
patterns resolved on children
no (pattern) fill", y=.8)

################################################################################
## More groups and (stroked and filled) paths 
library(grid)
r1 <- rectGrob(x=0, y=0, width=.5, height=.5, just=c("left", "bottom"))
r2 <- rectGrob(x=1, y=1, width=.75, height=.75, just=c("right", "top"),
               gp=gpar(fill="black"))

## Path with hole filled with pattern, group = FALSE
## Path is a "single shape" so result should be same as group = TRUE
grid.newpage()
grid.fill(gTree(children=gList(r1, r2)),
          rule="evenodd",
          gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("path from two rects
group = FALSE
pattern fill resolved on bbox of both rects", y=.8)

## Remove r2 from r1 with "group" and fill with gradient, group = FALSE
## Gradient should be applied to individual rects
grid.newpage()
grid.group(r2, "dest.out", r1, gp=gpar(fill=linearGradient(group=FALSE)))
HersheyLabel("group of two rects
group = FALSE
big rect takes bite out of small rect
pattern fill resolved on each rect", y=.8)

## fill on the grob in the group
grid.newpage()
grid.define(r2, "dest.out",
            editGrob(r1, gp=gpar(fill=linearGradient())),
            name="r1")
pushViewport(viewport(x=1, y=1))
grid.use("r1")
upViewport()
HersheyLabel("group use imposes transformation
rect within group has pattern fill
pattern resolved on rect on use", y=.2)
## ... even with scaling (as well as translation) transformation
grid.newpage()
grid.define(r2, "dest.out",
            editGrob(r1, gp=gpar(fill=linearGradient())),
            name="r1")
pushViewport(viewport(x=1, y=1, width=.5, height=.5))
grid.use("r1")
upViewport()
HersheyLabel("group use imposes transformation AND scaling
rect within group has pattern fill
pattern resolved on rect on use", y=.2)

## fill on the grob in the group, group = FALSE
grid.newpage()
grid.define(r2, "dest.out",
            editGrob(r1, gp=gpar(fill=linearGradient(group=FALSE))),
            name="gt")
pushViewport(viewport(x=1, y=1))
grid.use("gt")
upViewport()
HersheyLabel("group use imposes transformation
rect within group has pattern fill
group = FALSE (no effect)
pattern resolved on rect on use", y=.2)
## ... even with scaling (as well as translation) transformation, group=FALSE
grid.newpage()
grid.define(r2, "dest.out",
            editGrob(r1, gp=gpar(fill=linearGradient(group=FALSE))),
            name="gt")
pushViewport(viewport(x=1, y=1, width=.5, height=.5))
grid.use("gt")
upViewport()
HersheyLabel("group use imposes transformation AND scaling
rect within group has pattern fill
group = FALSE (no effect)
pattern resolved on rect on use", y=.2)

## Test gTree with pattern fill with children that push vp, group = FALSE
## SO child with vp should get different fill
gt <- gTree(children=gList(rectGrob(),
                           rectGrob(vp=viewport(width=.5, height=.5))),
            gp=gpar(fill=linearGradient(group=FALSE)))
grid.newpage()
grid.draw(gt)
HersheyLabel("gTree with two rects
one rect has vp
fill resolved on each rect
rects get different fill")

## gTree with group as child, fill resolved on gTree bbox
## (so needs group bbox)
grid.newpage()
group <- groupGrob(r1)
gt <- gTree(children=gList(r2, group),
            gp=gpar(fill=linearGradient()))
grid.draw(gt)
HersheyLabel("gTree has group as child
gTree has pattern fill
pattern resolved on gTree", y=.2)

## gTree with group USE as child, fill resolved on gTree bbox
## (so needs group USE bbox)
grid.newpage()
r3 <- rectGrob(width=.5, height=.5)
group <- grid.define(r1, name="r")
use <- useGrob("r", vp=viewport(1, 1))
gt <- gTree(children=gList(r3, use),
            gp=gpar(fill=linearGradient()))
grid.rect(.25, .25, .75, .75, just=c("left", "bottom"),
          gp=gpar(col=NA, fill=linearGradient()))
grid.draw(gt)
HersheyLabel("gTree has group USE as child
gTree has pattern fill
pattern resolved on gTree
(rect behind shows correct gradient)", y=.2)

## Check grobCoords() from transform with skew produces same outline
grid.newpage()
c <- circleGrob(r=c(.3, .4))
pts <- grobCoords(c, closed=TRUE)
p <- pathGrob(c(pts[[1]]$x, pts[[2]]$x),
              c(pts[[1]]$y, pts[[2]]$y),
              default.units="in",
              id=rep(1:2, each=100),
              rule="evenodd",
              gp=gpar(fill="grey"))
grid.draw(p)
grid.define(p, name="path")
use <- useGrob("path",
               transform=function(group, ...)
                   viewportTransform(group,
                                     shear=groupShear(.5),
                                     ...))
newPts <- grobCoords(use, closed=TRUE)
newPath <- circleGrob(c(newPts[[1]][[1]][[1]]$x, newPts[[1]][[1]][[2]]$x),
                      c(newPts[[1]][[1]][[1]]$y, newPts[[1]][[1]][[2]]$y),
                      default.units="in",
                      r=unit(.5, "mm"),
                      gp=gpar(col="red", fill="red"))
grid.draw(use)
grid.draw(newPath)

