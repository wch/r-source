
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

################################################################################
## Need to test ...

##   mask with clip
##   clip with mask
##   mask with gradient
##   mask with mask

##   inheriting mask
##   pushing and popping masks
##   pushing and popping no masks

##   display list replays
##   display resizes
##   grid.grab()
##   recordPlot()/replayPlot()
