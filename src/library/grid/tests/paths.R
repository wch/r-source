
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
## Paths that produce same result as normal drawing

grid.newpage()
grid.stroke(circleGrob())
HersheyLabel("Single circle stroked
same as normal drawing")

grid.newpage()
grid.stroke(circleGrob(1:2/3, r=.3))
HersheyLabel("Two overlapping circles stroked
same as normal drawing")

grid.newpage()
grid.stroke(circleGrob(1:2/3, r=.1))
HersheyLabel("Two distinct circles stroked
same as normal drawing")

grid.newpage()
grid.fillStroke(circleGrob(1:2/3, r=.3), gp=gpar(col=NA, fill="grey"))
HersheyLabel("Two overlapping circles filled ONLY
non-zero winding rule
same as normal drawing")

grid.newpage()
grid.fillStroke(circleGrob(1:2/3, r=.3), gp=gpar(fill="grey"))
HersheyLabel("Two overlapping circles filled and stroked
non-zero winding rule
same as normal drawing")

grid.newpage()
grid.fillStroke(circleGrob(r=c(.1, .3)), gp=gpar(fill="grey"))
HersheyLabel("Two nested circles stroked and filled
non-zero winding rule
same as normal drawing")

################################################################################
## Paths that produce DIFFERENT result compared to normal drawing

grid.newpage()
grid.stroke(circleGrob(gp=gpar(fill="grey")))
HersheyLabel("Filled circle stroked
NO fill!")

grid.newpage()
grid.fill(circleGrob(1:2/3, r=.3), gp=gpar(fill="grey"))
HersheyLabel("Two overlapping circles filled
non-zero winding rule
NO stroke!")

grid.newpage()
grid.segments(gp=gpar(lwd=10, col=4))
grid.fill(circleGrob(1:2/3, r=.3), gp=gpar(fill=rgb(1,0,0,.5)))
HersheyLabel("Two overlapping circles filled
non-zero winding rule
semitransparent fill
NO overlap!")

grid.newpage()
grid.fill(circleGrob(1:2/3, r=.3), gp=gpar(fill="grey"), rule="evenodd")
HersheyLabel("Two overlapping circles filled
even-odd rule
get a HOLE!")

grid.newpage()
grid.fillStroke(circleGrob(1:2/3, r=.3), gp=gpar(fill="grey"), rule="evenodd")
HersheyLabel("Two overlapping circles stroked and filled
even-odd rule
get a HOLE!")

grid.newpage()
grid.fillStroke(circleGrob(r=c(.1, .3)), gp=gpar(fill="grey"), rule="evenodd")
HersheyLabel("Two nested circles stroked and filled
even-odd rule
get a HOLE!")

grid.newpage()
grid.fillStroke(polylineGrob(c(.2, .5, .8), c(.2, .8, .2)),
                gp=gpar(lwd=5, fill="grey"))
HersheyLabel("Polyline (open)
stroked and filled
get filled region
(even though not closed)")
   
grid.newpage()
pushViewport(viewport(clip=as.path(circleGrob(r=c(.3, .1)), rule="evenodd")))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("Clipping path is nested circles
even-odd rule
clip filled rect
get donut", y=.7)

grid.newpage()
grid.stroke(textGrob("testing", gp=gpar(fontface="bold", cex=4)))
HersheyLabel("Path based on text
stroke produces glyph outlines", y=.8)

grid.newpage()
gt <- gTree(children=gList(circleGrob(),
                           textGrob("testing", gp=gpar(fontface="bold", cex=4)),
                           rectGrob(width=.8, height=.5)))
grid.fillStroke(gt, gp=gpar(fill="grey"), rule="evenodd")
HersheyLabel("Path based on circle and text and rect
stroked and filled with even-odd rule
all outlines and glyphs stroked and
glyphs filled and
space between circle and rect filled
(PDF will NOT draw text)", y=.85)

grid.newpage()
gt <- gTree(children=gList(textGrob("testing", gp=gpar(fontface="bold", cex=4)),
                           circleGrob(),
                           rectGrob(width=.8, height=.5)))
grid.fillStroke(gt, gp=gpar(fill="grey"), rule="evenodd")
HersheyLabel("Path based on text and circle and rect
stroked and filled with even-odd rule
all outlines and glyphs stroked and
glyphs filled and
space between circle and rect filled
(PDF will ONLY draw text)", y=.85)

grid.newpage()
grid.fill(pathGrob(c(.2, .2, .8, .8, .4, .4, .6, .6),
                   c(.2, .8, .8, .2, .4, .6, .6, .4),
                   id.lengths=c(4, 4),
                   rule="winding"),
          gp=gpar(fill="grey"))
HersheyLabel("Path based on pathGrob
same fill rule for both (winding)
no hole")

grid.newpage()
grid.fill(pathGrob(c(.2, .2, .8, .8, .4, .4, .6, .6),
                   c(.2, .8, .8, .2, .4, .6, .6, .4),
                   id.lengths=c(4, 4),
                   rule="winding"),
          rule="evenodd",
          gp=gpar(fill="grey"))
HersheyLabel("Path based on pathGrob
DIFFERENT fill rules
(grobPath has winding but path has even-odd)
get a hole!")

## Gradient in path
grid.newpage()
grad <- linearGradient(y1=.5, y2=.5)
grid.fill(circleGrob(1:2/3, r=.1), gp=gpar(fill=grad))
HersheyLabel("path (two circles) with gradient fill
gradient relative to circle bounds
(black at left to white at right)", y=.8)

## Pattern in path
grid.newpage()
rects <- gTree(children=gList(rectGrob(width=unit(5, "mm"),
                                       height=unit(5, "mm"),
                                       just=c("left", "bottom"),
                                       gp=gpar(fill="black")),
                              rectGrob(width=unit(5, "mm"),
                                       height=unit(5, "mm"),
                                       just=c("right", "top"),
                                       gp=gpar(fill="black"))))
checkerBoard <- pattern(rects,
                        width=unit(1, "cm"), height=unit(1, "cm"),
                        extend="repeat")
grid.fill(circleGrob(1:2/3, r=.1), gp=gpar(fill=checkerBoard))
HersheyLabel("path (two circles) with pattern fill", y=.8)
    
## Path that is clipped
grid.newpage()
pushViewport(viewport(clip=circleGrob(r=.2)))
grid.fill(circleGrob(1:2/3, r=.1), gp=gpar(fill="grey"))
popViewport()
HersheyLabel("filled path (two circles)
clipped by circle path
result is two half-moons", .8)
    
## path that is masked
grid.newpage()
pushViewport(viewport(mask=devMask(circleGrob(r=.2, gp=gpar(fill="black")),
                                   circleGrob(r=.2, gp=gpar(col="white",
                                                            fill="white")))))
grid.fill(circleGrob(1:2/3, r=.1), gp=gpar(fill="grey"))
popViewport()
HersheyLabel("filled path (two circles)
masked by filled circle 
result is two half-moons", .8)

## raster-based pattern in path
grid.newpage()
checkerBoard <- pattern(rasterGrob(matrix(c(0:1, 1:0), nrow=2),
                                          width=.2, height=.2,
                                          interpolate=FALSE),
                        width=.2, height=.2,
                        extend="repeat")
grid.fill(circleGrob(1:2/3, r=.1), gp=gpar(fill=checkerBoard))
HersheyLabel("path (two circles) with pattern fill
pattern is (repeated) raster
(pattern size/shape relative to circle bbox)", y=.8)
    



################################################################################
## TODO

notrun <- function() {

## UTF8 text in path
    
} ## notrun()
