
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
## Nesting of patterns, clipping paths, masks, groups, and paths
## ALL disallowed within a path or clipping path

grid.newpage()
pat <- pattern(circleGrob(r=unit(.5, "cm"), gp=gpar(fill="black")),
               width=unit(2, "cm"), height=unit(2, "cm"),
               extend="repeat")
path <- circleGrob(gp=gpar(fill=pat))
pushViewport(viewport(clip=path))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("clipping path is based on circle
circle has (tiling) pattern fill
result is grey circle")

grid.newpage()
pat <- linearGradient()
path <- circleGrob(gp=gpar(fill=pat))
pushViewport(viewport(clip=path))
grid.rect(gp=gpar(fill="grey"))
HersheyLabel("clipping path is based on circle
circle has gradeint fill
result is grey circle")

grid.newpage()
pat <- pattern(circleGrob(r=unit(.5, "cm"), gp=gpar(fill="black")),
               width=unit(2, "cm"), height=unit(2, "cm"),
               extend="repeat")
path <- circleGrob(gp=gpar(fill=pat))
grid.fillStroke(path, gp=gpar(fill="grey"))
HersheyLabel("path is based on circle
circle has (tiling) pattern fill
path is filled grey
result is filled grey circle
(pattern fill silently ignored)")

grid.newpage()
cpath <- circleGrob(r=.2)
path <- circleGrob(vp=viewport(clip=cpath))
grid.fillStroke(path, gp=gpar(fill="grey"))
HersheyLabel("path is based on circle
circle has viewport with
clipping path based on smaller circle
result is filled grey circle
(clipping path ignored with warning)")

grid.newpage()
mask <- circleGrob(r=.2, gp=gpar(fill="black"))
path <- circleGrob(vp=viewport(mask=mask))
grid.fillStroke(path, gp=gpar(fill="grey"))
HersheyLabel("path is based on circle
circle has viewport with
mask based on smaller circle
result is filled grey circle
(mask ignored with warning)")

grid.newpage()
group <- groupGrob(circleGrob(r=.2))
path <- gTree(children=gList(circleGrob(), group))
grid.fillStroke(path, gp=gpar(fill="grey"))
HersheyLabel("path is based on circle AND group
(group is smaller circle)
result is filled grey circle
(group ignored with warning)")

grid.newpage()
grid.define(circleGrob(r=.2), name="g")
path <- gTree(children=gList(circleGrob(), useGrob("g")))
grid.fillStroke(path, gp=gpar(fill="grey"))
HersheyLabel("path is based on circle AND group *use*
(group is smaller circle)
result is filled grey circle
(group ignored with warning)")

grid.newpage()
subpath <- strokeGrob(circleGrob(r=.2))
path <- gTree(children=gList(circleGrob(), subpath))
grid.fillStroke(path, gp=gpar(fill="grey"))
HersheyLabel("path is based on circle AND subpath
(subpath is smaller circle)
result is filled grey circle
(subpath ignored with warning)")

grid.newpage()
subpath <- strokeGrob(circleGrob(r=.2))
path <- gTree(children=gList(subpath, circleGrob()))
grid.fillStroke(path, gp=gpar(fill="grey"))
HersheyLabel("path is based on subpath AND circle
(subpath is smaller circle)
result is filled grey circle
(subpath ignored with warning)")


################################################################################
## TODO

notrun <- function() {

} ## notrun()
