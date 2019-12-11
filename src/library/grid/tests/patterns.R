
library(grid)

################################################################################
## Gradients

## Simple linear gradient on grob
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))

## Test linearGradient() arguments
grid.newpage()
grid.rect(gp=gpar(fill=linearGradient(c("red", "yellow", "red"),
                                      c(0, .5, 1),
                                      x1=.5, y1=unit(1, "in"), 
                                      x2=.5, y2=1,
                                      extend="none")))

## Gradient relative to grob
grid.newpage()
grid.rect(width=.5, height=.5,
          gp=gpar(fill=linearGradient()))

## Gradient on viewport
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()

## Gradient relative to viewport
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect(width=.5, height=.5)

grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()

## Inherited gradient on viewport
## (should be relative to first, larger viewport)
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
pushViewport(viewport(width=.5, height=.5))
grid.rect()

## Restore of gradient (just like any other gpar)
grid.newpage()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect(x=.2, width=.2, height=.5)
pushViewport(viewport(gp=gpar(fill="green")))
grid.rect(x=.5, width=.2, height=.5)
popViewport()
grid.rect(x=.8, width=.2, height=.5)

## Translucent gradient
grid.newpage()
grid.text("Reveal", gp=gpar(fontface="bold", cex=3))
grid.rect(gp=gpar(fill=linearGradient(c("white", "transparent"),
                                      y1=.5, y2=.5)))

## Radial gradient
grid.newpage()
grid.rect(gp=gpar(fill=radialGradient()))    

## Test radialGradient() arguments
grid.newpage()
grid.rect(gp=gpar(fill=radialGradient(c("white", "black"),
                                      cx1=.8, cy1=.8)))    

## Gradient on a gTree
grid.newpage()
grid.draw(gTree(children=gList(rectGrob(gp=gpar(fill=linearGradient())))))

## Rotated gradient
grid.newpage()
pushViewport(viewport(width=.5, height=.5, angle=45,
                      gp=gpar(fill=linearGradient())))
grid.rect()

######################################
## Tests of replaying graphics engine display list

## Resize graphics device
dev.new()
grid.rect(gp=gpar(fill=linearGradient()))

dev.new()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()

## Copy to new graphics device
dev.new()
grid.rect(gp=gpar(fill=linearGradient()))
x <- recordPlot()
dev.new()
x
## (Resize that as well if you like)

dev.new()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
x <- recordPlot()
dev.new()
x

## Replay on new device with gradient already defined
## (watch out for recorded grob using existing gradient)
dev.new()
grid.rect(gp=gpar(fill=linearGradient()))
x <- recordPlot()
dev.new()
grid.rect(gp=gpar(fill=linearGradient(c("white", "red"))))
x

## Similar to previous, except involving viewports
dev.new()
pushViewport(viewport(gp=gpar(fill=linearGradient())))
grid.rect()
x <- recordPlot()
dev.new()
pushViewport(viewport(gp=gpar(fill=linearGradient(c("white", "red")))))
grid.rect()
x
}

######################################
## Test of 'grid' display list

grid.newpage()
grid.rect(name="r")
grid.edit("r", gp=gpar(fill=linearGradient()))

grid.newpage()
grid.rect(gp=gpar(fill=linearGradient()))
x <- grid.grab()
dev.new()
grid.draw(x)

grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()
x <- grid.grab()
dev.new()
grid.draw(x)


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

## ONCE for multiple rects from single grob
grid.newpage()
grid.rect(x=1:5/6, y=1:5/6, width=1/8, height=1/8,
          gp=gpar(fill=linearGradient()))

## ONCE for viewport with rect
grid.newpage()
pushViewport(viewport(width=.5, height=.5, gp=gpar(fill=linearGradient())))
grid.rect()

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

######################################
## Test for running out of patterns

## Should NOT run out of patterns
grid.newpage()
for (i in 1:21) {
    grid.rect(gp=gpar(fill=linearGradient()))
}

## Should run out of patterns
grid.newpage()
for (i in 1:21) {
    try(pushViewport(viewport(gp=gpar(fill=linearGradient()))))
}

## grid.newpage() should fix it
grid.newpage()
for (i in 1:21) {
    grid.rect(gp=gpar(fill=linearGradient()))
}


