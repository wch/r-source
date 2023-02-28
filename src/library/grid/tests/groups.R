
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
## Basic sanity checks

## Simple group grob (same as a normal grob)
grid.newpage()
grid.group(rectGrob(width=.5, height=.5, gp=gpar(fill="grey")))
HersheyLabel("Group = single rect
half-width, half-height, grey fill
same as normal rect")


################################################################################
## Compositing operators

source("compositing.R")


################################################################################
## Isolated groups

## Apply mask to isolated group
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
grid.rect(height=.5, gp=gpar(fill="red"))
grid.rect(width=.5, gp=gpar(fill="blue"))
HersheyLabel("no mask
each shape composite over")
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2),
             viewport(mask=devMask(circleGrob(gp=gpar(col=NA,
                                                      fill=rgb(0,0,0,.5))),
                                   circleGrob(gp=gpar(col=NA,
                                                      fill=grey(.5))))))
grid.rect(height=.5, gp=gpar(fill="red"))
grid.rect(width=.5, gp=gpar(fill="blue"))
popViewport()
HersheyLabel("mask
each shape masked then composite over")
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1),
             viewport(mask=devMask(circleGrob(gp=gpar(col=NA,
                                                      fill=rgb(0,0,0,.5))),
                                   circleGrob(gp=gpar(col=NA,
                                                      fill=grey(.5))))))
grid.group(rectGrob(width=.5, gp=gpar(fill="blue")),
               "over",
               rectGrob(height=.5, gp=gpar(fill="red")))
popViewport()
HersheyLabel("mask and group
each shape composite over in group
group masked")
popViewport()


## Grobs that draw more than one shape
## Circle of small circles, draw over and then compose with alpha mask
grid.newpage()
t <- seq(30, 360, 30)
x <- .5 + .3*cos(pi*t/180)
y <- .5 + .3*sin(pi*t/180)
circles <- circleGrob(x, y, r=.1, gp=gpar(fill="black"))
pushViewport(viewport(layout=grid.layout(2, 2)))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
grid.draw(circles)
HersheyLabel("circle grob")
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2,
                      mask=devMask(rectGrob(gp=gpar(col=NA,
                                                    fill=rgb(0,0,0,.5))),
                                   rectGrob(gp=gpar(col=NA,
                                                    fill=grey(.5))))))
grid.draw(circles)
HersheyLabel("masked
circle grob")
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.group(circles)
HersheyLabel("group
circle grob")
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2,
                      mask=devMask(rectGrob(gp=gpar(col=NA,
                                                    fill=rgb(0,0,0,.5))),
                                   rectGrob(gp=gpar(col=NA,
                                                    fill=grey(.5))))))
grid.group(circles)
HersheyLabel("masked
group
circle grob")
popViewport()


################################################################################
## Inheritance of graphics settings

## Compositing groups inherit graphics settings from "parent"
grid.newpage()
pushViewport(viewport(gp=gpar(fill="pink")))
grid.rect(x=1/4, width=1/2)
grid.group(rectGrob(x=3/4, width=1/2))
HersheyLabel("group inherits current gpar
(right rect pink)")
popViewport()

## Watch out for alpha level accumulating
grid.newpage()
pushViewport(viewport(gp=gpar(fill=rgb(1,0,0,.5))))
grid.rect(x=1/4, width=1/2)
grid.group(rectGrob(x=3/4, width=1/2))
HersheyLabel("group inherits current gpar
(right rect trans red)
test with alpha")
popViewport()


################################################################################
## gTrees

## The trick here is that the rectangle is drawn within a viewport,
## which means that there is an upViewport() within the drawing
## of the group, which means 'grid' has to have guards against that
## upViewport() resetting any mask that is in effect outside the group.
##
## This is a proxy for drawing complex things like 'ggplot2' plot gTrees
## within a group (which also works, I just cannot make it a test within
## the 'grid' package).
grid.newpage()
gt <- gTree(children=gList(rectGrob(vp=viewport()),
                           circleGrob(1:2/3, r=.3, gp=gpar(fill="black"))))
pushViewport(viewport(mask=devMask(rectGrob(gp=gpar(col=NA,
                                                    fill=rgb(0,0,0,.5))),
                                   rectGrob(gp=gpar(col=NA,
                                                    fill=grey(.5))))))
grid.group(gt)
HersheyLabel("group is gTree with child with vp
group drawn with mask", y=.9)
HersheyLabel("mask NOT reset within group definition
(circles NOT masked within group)", y=.1)
popViewport()


################################################################################
## Reuse

## Use group in smaller viewport (translate and scale)
grid.newpage()
grid.group(rectGrob(width=unit(1, "in"), height=unit(1, "in"),
                     gp=gpar(fill="grey")),
            name="rectGroup")
HersheyLabel("draw named group", y=.75)
pushViewport(viewport(x=.75, y=.25, width=.5, height=.5))
grid.rect(gp=gpar(fill=NA, lty="dashed"))
grid.use("rectGroup")
HersheyLabel("reuse group
(in smaller viewport)
shrunk AND thinner border", y=.25)
popViewport()

## Just translate
grid.newpage()
grid.define(rectGrob(width=unit(1, "in"), height=unit(1, "in"),
                     gp=gpar(fill="grey")),
            name="rectGroup")
grid.use("rectGroup")
HersheyLabel("draw named group", y=.75)
pushViewport(viewport(x=.75, y=.25, width=.5, height=.5))
grid.rect(gp=gpar(fill=NA, lty="dashed"))
grid.use("rectGroup", viewportTranslate)
HersheyLabel("reuse group
just with translate transform
(in smaller viewport)
just shifted", y=.25)
popViewport()

## Just scale
grid.newpage()
grid.define(rectGrob(width=unit(1, "in"), height=unit(1, "in"),
                     gp=gpar(fill="grey")),
            name="rectGroup")
grid.use("rectGroup")
HersheyLabel("draw named group", y=.75)
pushViewport(viewport(x=.75, y=.25, width=.5, height=.5))
grid.rect(gp=gpar(fill=NA, lty="dashed"))
grid.use("rectGroup", viewportScale)
HersheyLabel("reuse group
just with scale transform
(in smaller viewport)
just scaled", y=.25)
popViewport()

## Just translate BUT viewport x/y is NOT "centre"
grid.newpage()
grid.define(rectGrob(width=unit(1, "in"), height=unit(1, "in"),
                     gp=gpar(fill="grey")),
            name="rectGroup")
grid.use("rectGroup")
HersheyLabel("draw named group", y=.75)
pushViewport(viewport(x=1, y=0, width=.5, height=.5, just=c("right", "bottom")))
grid.rect(gp=gpar(fill=NA, lty="dashed"))
grid.use("rectGroup", viewportTranslate)
HersheyLabel("reuse group
just with translate transform
with vp x/y at bottom-right
(in smaller viewport)
shifted to new vp x/y", y=.25)
popViewport()

## Just scale (which ignores the fact that x/y is NOT "centre")
grid.newpage()
grid.define(rectGrob(width=unit(1, "in"), height=unit(1, "in"),
                     gp=gpar(fill="grey")),
            name="rectGroup")
grid.use("rectGroup")
HersheyLabel("draw named group", y=.75)
pushViewport(viewport(x=1, y=0, width=.5, height=.5, just=c("right", "bottom")))
grid.rect(gp=gpar(fill=NA, lty="dashed"))
grid.use("rectGroup", viewportScale)
HersheyLabel("reuse group
just with scale transform
with vp x/y at bottom-right
(in smaller viewport)
just scaled
(new vp x/y ignored)", y=.25)
popViewport()

## Rotation
grid.newpage()
grid.define(rectGrob(width=unit(1, "in"), height=unit(1, "in"),
                     gp=gpar(fill="grey")),
            name="rectGroup")
grid.use("rectGroup")
pushViewport(viewport(angle=15))
grid.rect(gp=gpar(lty="dashed", fill=NA))
grid.use("rectGroup")
popViewport()
HersheyLabel("reuse group
rotated viewport
rotated group", y=.25)

## Shear 
grid.newpage()
grid.define(rectGrob(width=unit(1, "in"), height=unit(1, "in"),
                     gp=gpar(fill="grey")),
            name="rectGroup")
grid.use("rectGroup")
trans <- function(group, ...) {
    viewportTransform(group, shear=groupShear(.5, 0))
}
grid.use("rectGroup", transform=trans)
HersheyLabel("reuse group
shear (skewX) transform
sheared group", y=.25)

grid.newpage()
grid.define(rectGrob(width=unit(1, "in"), height=unit(1, "in"),
                     gp=gpar(fill="grey")),
            name="rectGroup")
grid.use("rectGroup")
trans <- function(group, ...) {
    viewportTransform(group, shear=groupShear(0, .5))
}
grid.use("rectGroup", transform=trans)
HersheyLabel("reuse group
shear (skewY) transform
sheared group", y=.25)

grid.newpage()
pushViewport(viewport(gp=gpar(col=2)))
grid.define(circleGrob(r=.1), name="c", gp=gpar(lwd=5))
grid.use("c")
pushViewport(viewport(x=.25, gp=gpar(col="green")))
grid.use("c")
popViewport()
pushViewport(viewport(x=.75, gp=gpar(col="blue")))
grid.use("c")
popViewport()
popViewport()
HersheyLabel("define circle (no colour)
(red from viewport)
left viewport sets green
(circle stays red)
right viewport sets blue
(cirlce stays red)", y=.25)

grid.newpage()
grid.define(rectGrob(gp=gpar(col=NA, fill=radialGradient())),
            name="gradient")
grid.use("gradient")
pushViewport(viewport(y=.1, height=.2))
grid.rect(gp=gpar(lty="dashed", fill=NA))
grid.use("gradient")
popViewport()
HersheyLabel("group with gradient fill
reused in viewport with different scale
distorted gradient", y=.9)

################################################################################
## Combinations (masks in groups, groups in groups, ...)

## Text in group
grid.newpage()
group <- gTree(children=gList(rectGrob(width=.8, height=.5),
                              textGrob("testing", gp=gpar(cex=3))))
grid.group(group)
HersheyLabel("group with text", y=.8)
    
## Pattern in group
grid.newpage()
grid.group(rectGrob(width=.8, height=.5, gp=gpar(fill=linearGradient())))
HersheyLabel("group with gradient fill", y=.8)

## Clipping path in group
grid.newpage()
vp <- viewport(clip=circleGrob())
grid.group(rectGrob(gp=gpar(fill="grey")), vp=vp)
HersheyLabel("group with vp
vp is viewport with clipping path
result is grey circle")

grid.newpage()
vp <- viewport(clip=circleGrob())
grid.group(rectGrob(gp=gpar(fill="grey"), vp=vp))
HersheyLabel("group with grob with vp
vp is viewport with clipping path
result is grey circle")
           
## Mask in group
grid.newpage()
vp <- viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                            circleGrob(gp=gpar(col="white", fill="white"))))
grid.group(rectGrob(gp=gpar(fill="grey")), vp=vp)
HersheyLabel("group with vp
vp is viewport with mask
result is grey circle")

grid.newpage()
vp <- viewport(mask=devMask(circleGrob(gp=gpar(fill="black")),
                            circleGrob(gp=gpar(col="white", fill="white"))))
grid.group(rectGrob(gp=gpar(fill="grey"), vp=vp))
HersheyLabel("group with grob with vp
vp is viewport with mask
result is grey circle")
    
## Path in group
grid.newpage()
grid.group(fillGrob(circleGrob(1:2/3, r=.3), gp=gpar(fill="grey")))
HersheyLabel("group with (filled) path")
    
## Group in group
grid.newpage()
gt <- gTree(children=gList(rectGrob(width=.5, gp=gpar(fill=rgb(1,0,0,.5))),
                           groupGrob(rectGrob(height=.5,
                                              gp=gpar(fill=rgb(0,0,1,.5))))))
grid.group(gt)
HersheyLabel("group containing group
both groups semitransparent
see overlap between groups")

grid.newpage()
rects <- gTree(children=gList(rectGrob(width=unit(5, "mm"),
                                       height=unit(5, "mm"),
                                       just=c("left", "bottom"),
                                       gp=gpar(fill="black")),
                              rectGrob(width=unit(5, "mm"),
                                       height=unit(5, "mm"),
                                       just=c("right", "top"),
                                       gp=gpar(fill="black"))))
pat <- pattern(rects,
               width=unit(1, "cm"), height=unit(1, "cm"),
               extend="repeat")
grid.rect(gp=gpar(fill=pat))
gt1 <- gTree(children=gList(rectGrob(width=.5, gp=gpar(fill=2)),
                            rectGrob(height=.5, gp=gpar(fill=4))))
vp <- viewport(mask=devMask(rectGrob(gp=gpar(col=NA, fill=rgb(0,0,0,.7))),
                            rectGrob(gp=gpar(col=NA, fill=grey(.7)))))
g1 <- groupGrob(gt1, vp=vp)
gt2 <- gTree(children=gList(g1, segmentsGrob(gp=gpar(lwd=100))))
g2 <- groupGrob(gt2, vp=vp)
grid.draw(g2)
grid.rect(width=.8, height=.4, gp=gpar(col=NA, fill=rgb(1,1,1,.7)))
HersheyLabel("group containing group
inner group overlapping rects
inner group drawn with semitransparent mask
(see through blue rect - no overlap visible)
outer group thick black line
outer group drawn with semitransparent mask
(see through black line - no overlap visible)
[white rect just background for this label]")

## Group used with transform in group used with transform
grid.newpage()
vp1 <- viewport(x=.3, y=.2, width=.3, height=.3)
vp2 <- viewport(x=.5, y=.6, width=.1, height=.3)
vp3 <- viewport(x=.7, y=.2, width=.3, height=.3)
pushViewport(vp1)
grid.rect(gp=gpar(col="grey"))
grid.define(polygonGrob(c(.2, .5, .8), c(.2, .8, .2),
                        gp=gpar(lwd=10, col=2, fill=3)),
            name="p")
grid.use("p")
popViewport()
pushViewport(vp2)
grid.rect(gp=gpar(col="grey"))
grid.use("p")
grid.define(useGrob("p"), name="g")
popViewport()
pushViewport(vp3)
grid.rect(gp=gpar(col="grey"))
grid.use("g")
popViewport()
HersheyLabel("bottom left is group-1 definition
top is reuse of group-1 with horiz squash
top is also group-2 definition
bottom right is reuse of group-2 with horiz stretch", y=.9)

grid.newpage()
vp1 <- viewport(x=.2, y=.2, width=.3, height=.3)
vp2 <- viewport(x=.3, y=.5, width=.1, height=.2, angle=-30)
vp3 <- viewport(x=.7, y=.7, width=.5, height=.5)
pushViewport(vp1)
grid.rect(gp=gpar(col="grey"))
grid.define(polygonGrob(c(.2, .5, .8), c(.2, .8, .2),
                        gp=gpar(lwd=10, col=2, fill=3)),
            name="r")
grid.use("r")
popViewport()
pushViewport(vp2)
grid.rect(gp=gpar(lty="dotted"))
popViewport()
g <- useGrob("r", vp=vp2)
grid.draw(g)
pushViewport(vp3)
grid.rect()
pushViewport(vp1)
grid.rect(gp=gpar(col="grey"))
popViewport()
pushViewport(vp2)
grid.rect(gp=gpar(lty="dotted"))
popViewport()
grid.define(g, name="g")
grid.use("g")
popViewport()
pushViewport(viewport(.7, .2, width=.5, height=.3))
grid.rect()
grid.use("g")
popViewport()
HersheyLabel("bottom-left is group-1 definition
group-1 reused with horiz. stretch and rotation
top-right is group-2 definition (based on group-1)
bottom-right is group-2 reused with vert. squash", y=.85)

## Raster in group
grid.newpage()
group <- gTree(children=gList(rectGrob(width=.8, height=.5),
                              rasterGrob(matrix(c(0:1, 1:0), nrow=2),
                                         width=.2, height=.2,
                                         interpolate=FALSE)))
grid.group(group)
HersheyLabel("group with raster", y=.8)
    
## Group with rect with gradient fill
grid.newpage()
grid.group(rectGrob(width=.5, height=.5,
                    gp=gpar(fill=linearGradient())))
HersheyLabel("group based on rect
rect has linear gradient", y=.1)

## Group with gradient fill
grid.newpage()
grid.group(rectGrob(width=.5, height=.5),
           gp=gpar(fill=linearGradient()))
HersheyLabel("group based on rect
GROUP has linear gradient", y=.1)

## Group with multiple shapes with gradient fill
grid.newpage()
grid.group(gTree(children=gList(rectGrob(.25, .25, .5, .5),
                                rectGrob(.75, .75, .5, .5))),
           gp=gpar(fill=linearGradient()))
HersheyLabel("group based on two rects
GROUP has linear gradient", y=.1)

## Subtler problem (need fill captured on definition)
grid.newpage()
grid.define(rectGrob(width=.5, height=.5),
            gp=gpar(fill=linearGradient()), name="g")
grid.use("g")
HersheyLabel("group based on rect
GROUP has linear gradient
group defined then used", y=.1)

## Group points/coords include src and dst even if they are not drawn
## (so linear gradient starts at bottom left of page NOT at centre of page)
grid.newpage()
grid.group(circleGrob(.25, .25, .5, gp=gpar(fill="black")),
           "dest.out",
           rectGrob(.75, .75, .5, .5),
           gp=gpar(fill=linearGradient()))
HersheyLabel("group based on circle and rect
circle clears rect
GROUP has linear gradient
gradient extent is rect AND circle extent", y=.3)

## Group with shapes that do NOT start at bottom-left
grid.newpage()
grid.group(circleGrob(3:4/5, 3:4/5, r=.1),
           gp=gpar(fill=linearGradient()))
HersheyLabel("group based on circles
GROUP has linear gradient
gradient extent is circle extent", y=.3)

################################################################################

TODO <- function() {

    
}
