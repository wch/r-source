
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

grid.newpage()
mask <- circleGrob(gp=gpar(col=NA,
                           fill=radialGradient(c("black", "transparent"))))
pushViewport(viewport(mask=mask))
grid.rect(width=.5, gp=gpar(fill="black"))
HersheyLabel("solid black rectangle with radial gradient mask", y=.1)

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
