library(grid)

## Tests for grobCoords()

check <- function(coords, model) {
    stopifnot(isTRUE(all.equal(as.numeric(coords$x), model$x)) &&
              isTRUE(all.equal(as.numeric(coords$y), model$y)))
}

## Simple primitive 
coords <- grobCoords(rectGrob(0, 0, 1, 1,
                              just=c("left", "bottom"),
                              default.units="in"),
                     closed=TRUE)
check(coords[[1]], list(x=c(0, 0, 1, 1), y=c(0, 1, 1, 0)))

## Primitives that generate more points than grob description
coords <- grobCoords(circleGrob(0, 0, r=unit(1, "in")), n=4,
                     closed=TRUE)
check(coords[[1]], list(x=c(1, 0, -1, 0), y=c(0, 1, 0, -1)))

coords <- grobCoords(xsplineGrob(c(0, 1, 2), c(0, 1, 0),
                                 default.units="in"),
                     closed=FALSE)
check(coords[[1]], list(x=c(0, 1, 2), y=c(0, 1, 0)))

## grob with 'id'
coords <- grobCoords(polylineGrob(1:4, 1:4,
                                  id=rep(1:2, each=2),
                                  default.units="in"),
                     closed=FALSE)
check(coords[[1]], list(x=1:2, y=1:2))
check(coords[[2]], list(x=3:4, y=3:4))

## grob with 'pathId'
coords <- grobCoords(pathGrob(c(0, 0, 3, 3, 1, 1, 2, 2, 4, 4, 7, 7, 5, 5, 6, 6),
                              c(0, 3, 3, 0, 1, 2, 2, 1, 4, 7, 7, 4, 5, 6, 6, 5),
                              id=rep(rep(1:2, each=4), 2),
                              pathId=rep(1:2, each=8),
                              default.units="in"),
                     closed=TRUE)
check(coords[[1]], list(x=c(0, 0, 3, 3), y=c(0, 3, 3, 0)))
check(coords[[2]], list(x=c(1, 1, 2, 2), y=c(1, 2, 2, 1)))
check(coords[[3]], list(x=c(4, 4, 7, 7), y=c(4, 7, 7, 4)))
check(coords[[4]], list(x=c(5, 5, 6, 6), y=c(5, 6, 6, 5)))

## Mostly testing makeContent()
coords <- grobCoords(bezierGrob(c(0, 1, 2, 3), c(0, 1, 2, 3),
                                default.units="in"),
                     closed=FALSE)
coords <- lapply(coords[[1]], function(x) { x[c(1, length(x))] })
check(coords, list(x=c(0, 3), y=c(0, 3)))

## Text returns a bounding box if closed is TRUE
coords <- grobCoords(textGrob("test", 0, 0, just=c("left", "bottom")),
                     closed=TRUE)
w <- convertWidth(stringWidth("test"), "in", valueOnly=TRUE)
h <- convertHeight(stringHeight("test"), "in", valueOnly=TRUE)
check(coords[[1]], list(x=c(0, 0, w, w), y=c(0, h, h, 0)))
      
coords <- grobCoords(textGrob("test"), closed=FALSE)
check(coords[[1]], emptyCoords)

## All emptyCoords
coords <- grobCoords(moveToGrob(), closed=FALSE)
check(coords[[1]], emptyCoords)

coords <- grobCoords(lineToGrob(), closed=FALSE)
check(coords[[1]], emptyCoords)

coords <- grobCoords(nullGrob(), closed=FALSE)
check(coords[[1]], emptyCoords)

coords <- grobCoords(clipGrob(), closed=FALSE)
check(coords[[1]], emptyCoords)

coords <- grobCoords(rasterGrob(matrix(1)), closed=FALSE)
check(coords[[1]], emptyCoords)

#################################
#################################
## Start a Cairo PNG device to get reliable results
png("temp.png", type="cairo")
#################################
#################################

#################################
## Names on coords
checkNames <- function(grob, seq, closed=TRUE) {
    pts <- grobCoords(grob, closed=closed)
    stopifnot(as.numeric(names(pts)) == seq)
}
## Circles
grob <- circleGrob(1:2/3)
checkNames(grob, 1:2)
## Lines
grob <- linesGrob(1:2, 1:2)
checkNames(grob, 1, FALSE)
## Polyline
grob <- polylineGrob(1:2, 1:2)
checkNames(grob, 1, FALSE)
grob <- polylineGrob(1:4, 1:4, id=rep(1:2, 2))
checkNames(grob, 1:2, FALSE)
## Polygon
grob <- polygonGrob(1:2, 1:2)
checkNames(grob, 1)
grob <- polygonGrob(1:4, 1:4, id=rep(1:2, 2))
checkNames(grob, 1:2)
## Path
grob <- pathGrob(1:2, 1:2)
checkNames(grob, 1)
grob <- pathGrob(1:4, 1:4, id=rep(1:2, 2))
checkNames(grob, c(1, 1))
grob <- pathGrob(1:4, 1:4, pathId=rep(1:2, 2))
checkNames(grob, 1:2)
grob <- pathGrob(1:8, 1:8, id=rep(1:2, 4), pathId=rep(1:2, each=4))
checkNames(grob, c(1, 1, 2, 2))
## Rects
grob <- rectGrob(unit(c(1, 3), "in"),
                 width=unit(1, "in"), height=unit(1, "in"))
checkNames(grob, 1:2)
## Segments
grob <- segmentsGrob(1:2)
checkNames(grob, 1:2, FALSE)
## Xsplines
grob <- xsplineGrob(1:2, 1:2)
checkNames(grob, 1, FALSE)
grob <- xsplineGrob(1:3, 1:3, open=FALSE)
checkNames(grob, 1)
grob <- xsplineGrob(1:4, 1:4, id=rep(1:2, 2))
checkNames(grob, 1:2, FALSE)
grob <- xsplineGrob(1:6, 1:6, id=rep(1:2, 3), open=FALSE)
checkNames(grob, 1:2)
## Text
grob <- textGrob(1:2)
checkNames(grob, 1)

#################################
## Points grobs

## Constants in grid.c
SMALL <- 0.25
RADIUS <- 0.375
SQRC <- 0.88622692545275801364
DMDC <- 1.25331413731550025119
TRC0 <- 1.55512030155621416073
TRC1 <- 1.34677368708859836060
TRC2 <- 0.77756015077810708036

## pch="."
grob <- pointsGrob(1:3, 1:3, default.units="in", pch=".")
coords <- grobCoords(grob, closed=TRUE)
x <- c(.995, .995, 1.005, 1.005)
y <- c(.995, 1.005, 1.005, .995)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=0
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=0)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1 - RADIUS, 1 - RADIUS, 1 + RADIUS, 1 + RADIUS)
y <- c(1 - RADIUS, 1 + RADIUS, 1 + RADIUS, 1 - RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=1
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=1)
coords <- grobCoords(grob, closed=TRUE, n=4)
mapply(check,
       lapply(coords, function(c) list(x=c$x[1], y=c$y[1])),
       list("1"=list(x=1 + RADIUS, y=1),
            "2"=list(x=2 + RADIUS, y=2),
            "3"=list(x=3 + RADIUS, y=3)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=2
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=2)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1, 1 + TRC1*RADIUS, 1 - TRC1*RADIUS)
y <- c(1 + TRC0*RADIUS, 1 - TRC2*RADIUS, 1 - TRC2*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=3
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=3)
coords <- grobCoords(grob, closed=TRUE)
stopifnot(isEmptyCoords(coords))
coords <- grobCoords(grob, closed=FALSE)
x1 <- c(1 - sqrt(2)*RADIUS, 1 + sqrt(2)*RADIUS)
y1 <- c(1, 1)
x2 <- c(1, 1)
y2 <- c(1 - sqrt(2)*RADIUS, 1 + sqrt(2)*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2)))
## pch=4
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=4)
coords <- grobCoords(grob, closed=TRUE)
stopifnot(isEmptyCoords(coords))
coords <- grobCoords(grob, closed=FALSE)
x1 <- c(1 - RADIUS, 1 + RADIUS)
y1 <- c(1 - RADIUS, 1 + RADIUS)
x2 <- c(1 - RADIUS, 1 + RADIUS)
y2 <- c(1 + RADIUS, 1 - RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2)))
## pch=5
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=5)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1 - sqrt(2)*RADIUS, 1, 1 + sqrt(2)*RADIUS, 1)
y <- c(1, 1 + sqrt(2)*RADIUS, 1, 1 - sqrt(2)*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=6
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=6)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1, 1 + TRC1*RADIUS, 1 - TRC1*RADIUS)
y <- c(1 - TRC0*RADIUS, 1 + TRC2*RADIUS, 1 + TRC2*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=7
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=7)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1 - RADIUS, 1 - RADIUS, 1 + RADIUS, 1 + RADIUS)
y <- c(1 - RADIUS, 1 + RADIUS, 1 + RADIUS, 1 - RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
x1 <- c(1 - RADIUS, 1 + RADIUS)
y1 <- c(1 - RADIUS, 1 + RADIUS)
x2 <- c(1 - RADIUS, 1 + RADIUS)
y2 <- c(1 + RADIUS, 1 - RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2)))
## pch=8
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=8)
coords <- grobCoords(grob, closed=TRUE)
stopifnot(isEmptyCoords(coords))
coords <- grobCoords(grob, closed=FALSE)
x1 <- c(1 - RADIUS, 1 + RADIUS)
y1 <- c(1 - RADIUS, 1 + RADIUS)
x2 <- c(1 - RADIUS, 1 + RADIUS)
y2 <- c(1 + RADIUS, 1 - RADIUS)
x3 <- c(1 - sqrt(2)*RADIUS, 1 + sqrt(2)*RADIUS)
y3 <- c(1, 1)
x4 <- c(1, 1)
y4 <- c(1 - sqrt(2)*RADIUS, 1 + sqrt(2)*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "1"=list(x=x3, y=y3), "1"=list(x=x4, y=y4),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "2"=list(x=x3+1, y=y3+1), "2"=list(x=x4+1, y=y4+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2),
            "3"=list(x=x3+2, y=y3+2), "3"=list(x=x4+2, y=y4+2)))
## pch=9
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=9)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1 - sqrt(2)*RADIUS, 1, 1 + sqrt(2)*RADIUS, 1)
y <- c(1, 1 + sqrt(2)*RADIUS, 1, 1 - sqrt(2)*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
x1 <- c(1 - sqrt(2)*RADIUS, 1 + sqrt(2)*RADIUS)
y1 <- c(1, 1)
x2 <- c(1, 1)
y2 <- c(1 - sqrt(2)*RADIUS, 1 + sqrt(2)*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2)))
## pch=10
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=10)
coords <- grobCoords(grob, closed=TRUE, n=4)
mapply(check,
       lapply(coords, function(c) list(x=c$x[1], y=c$y[1])),
       list("1"=list(x=1 + RADIUS, y=1),
            "2"=list(x=2 + RADIUS, y=2),
            "3"=list(x=3 + RADIUS, y=3)))
coords <- grobCoords(grob, closed=FALSE)
x1 <- c(1 - RADIUS, 1 + RADIUS)
y1 <- c(1, 1)
x2 <- c(1, 1)
y2 <- c(1 - RADIUS, 1 + RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2)))
## pch=11
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=11)
coords <- grobCoords(grob, closed=TRUE)
x1 <- c(1, 1 + TRC1*RADIUS, 1 - TRC1*RADIUS)
y1 <- c(1 - TRC0*RADIUS,
        1 + .5*(TRC2*RADIUS + TRC0*RADIUS),
        1 + .5*(TRC2*RADIUS + TRC0*RADIUS))
x2 <- c(1, 1 + TRC1*RADIUS, 1 - TRC1*RADIUS)
y2 <- c(1 + TRC0*RADIUS, 
        1 - .5*(TRC2*RADIUS + TRC0*RADIUS),
        1 - .5*(TRC2*RADIUS + TRC0*RADIUS))
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=12
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=12)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1 - RADIUS, 1 - RADIUS, 1 + RADIUS, 1 + RADIUS)
y <- c(1 - RADIUS, 1 + RADIUS, 1 + RADIUS, 1 - RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
x1 <- c(1 - RADIUS, 1 + RADIUS)
y1 <- c(1, 1)
x2 <- c(1, 1)
y2 <- c(1 - RADIUS, 1 + RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2)))
## pch=13
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=13)
coords <- grobCoords(grob, closed=TRUE, n=4)
mapply(check,
       lapply(coords, function(c) list(x=c$x[1], y=c$y[1])),
       list("1"=list(x=1 + RADIUS, y=1),
            "2"=list(x=2 + RADIUS, y=2),
            "3"=list(x=3 + RADIUS, y=3)))
coords <- grobCoords(grob, closed=FALSE)
x1 <- c(1 - RADIUS, 1 + RADIUS)
y1 <- c(1 - RADIUS, 1 + RADIUS)
x2 <- c(1 - RADIUS, 1 + RADIUS)
y2 <- c(1 + RADIUS, 1 - RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2)))
## pch=14
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=14)
coords <- grobCoords(grob, closed=TRUE)
x1 <- c(1 - RADIUS, 1 - RADIUS, 1 + RADIUS, 1 + RADIUS)
y1 <- c(1 - RADIUS, 1 + RADIUS, 1 + RADIUS, 1 - RADIUS)
x2 <- c(1, 1 + RADIUS, 1 - RADIUS)
y2 <- c(1 + RADIUS, 1 - RADIUS, 1 - RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x1, y=y1), "1"=list(x=x2, y=y2),
            "2"=list(x=x1+1, y=y1+1), "2"=list(x=x2+1, y=y2+1),
            "3"=list(x=x1+2, y=y1+2), "3"=list(x=x2+2, y=y2+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=15
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=15)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1 - RADIUS, 1 + RADIUS, 1 + RADIUS, 1 - RADIUS)
y <- c(1 - RADIUS, 1 - RADIUS, 1 + RADIUS, 1 + RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=16
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=16)
coords <- grobCoords(grob, closed=TRUE, n=4)
mapply(check,
       lapply(coords, function(c) list(x=c$x[1], y=c$y[1])),
       list("1"=list(x=1 + RADIUS, y=1),
            "2"=list(x=2 + RADIUS, y=2),
            "3"=list(x=3 + RADIUS, y=3)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=17
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=17)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1, 1 + TRC1*RADIUS, 1 - TRC1*RADIUS)
y <- c(1 + TRC0*RADIUS, 1 - TRC2*RADIUS, 1 - TRC2*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=18
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=18)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1 - RADIUS, 1, 1 + RADIUS, 1)
y <- c(1, 1 + RADIUS, 1, 1 - RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=19
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=19)
coords <- grobCoords(grob, closed=TRUE, n=4)
mapply(check,
       lapply(coords, function(c) list(x=c$x[1], y=c$y[1])),
       list("1"=list(x=1 + RADIUS, y=1),
            "2"=list(x=2 + RADIUS, y=2),
            "3"=list(x=3 + RADIUS, y=3)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=20
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=20)
coords <- grobCoords(grob, closed=TRUE, n=4)
mapply(check,
       lapply(coords, function(c) list(x=c$x[1], y=c$y[1])),
       list("1"=list(x=1 + SMALL, y=1),
            "2"=list(x=2 + SMALL, y=2),
            "3"=list(x=3 + SMALL, y=3)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=21
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=21)
coords <- grobCoords(grob, closed=TRUE)
mapply(check,
       lapply(coords, function(c) list(x=c$x[1], y=c$y[1])),
       list(list(x=1 + RADIUS, y=1),
            list(x=2 + RADIUS, y=2),
            list(x=3 + RADIUS, y=3)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=22
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=22)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1 - RADIUS*SQRC, 1 - RADIUS*SQRC, 1 + RADIUS*SQRC, 1 + RADIUS*SQRC)
y <- c(1 - RADIUS*SQRC, 1 + RADIUS*SQRC, 1 + RADIUS*SQRC, 1 - RADIUS*SQRC)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=23
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=23)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1, 1 + RADIUS*DMDC, 1, 1 - RADIUS*DMDC)
y <- c(1 - RADIUS*DMDC, 1, 1 + RADIUS*DMDC, 1)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=24
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=24)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1, 1 + TRC1*RADIUS, 1 - TRC1*RADIUS)
y <- c(1 + TRC0*RADIUS, 1 - TRC2*RADIUS, 1 - TRC2*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))
## pch=25
grob <- pointsGrob(1:3, 1:3, size=unit(1, "in"), default.units="in", pch=25)
coords <- grobCoords(grob, closed=TRUE)
x <- c(1, 1 + TRC1*RADIUS, 1 - TRC1*RADIUS)
y <- c(1 - TRC0*RADIUS, 1 + TRC2*RADIUS, 1 + TRC2*RADIUS)
mapply(check,
       coords,
       list("1"=list(x=x, y=y), "2"=list(x=x+1, y=y+1), "3"=list(x=x+2, y=y+2)))
coords <- grobCoords(grob, closed=FALSE)
stopifnot(isEmptyCoords(coords))

#################################
## coords from gTrees 
## gridCoords(c(.2, .2, .8, .8), c(.2, .8, .8, .2))
## gridPoints(list(gridCoords(c(.2, .2, .8, .8), c(.2, .8, .8, .2)),
##                 gridCoords(c(.4, .4, .6, .6), c(.4, .6, .6, .4))))
gtCheck <- function(x, y) {
    stopifnot(identical(x, y))
}
x <- c(.5, .5, 1.5, 1.5)
y <- c(.5, 1.5, 1.5, .5)
gtCheck(grobPoints(rectGrob(1, 1, 1, 1, default.units="in", name="r"),
                   closed=TRUE),
        gridGrobCoords(list("1"=gridCoords(x, y)), "r"))
gtCheck(grobPoints(rectGrob(1:2, 1, 1, 1,
                            default.units="in", name="r"),
                   closed=TRUE),
        gridGrobCoords(list("1"=gridCoords(x, y),
                            "2"=gridCoords(x + 1, y)),
                       "r"))
gtCheck(grobPoints(linesGrob(1:2, 1:2, default.units="in", name="l"),
                   closed=FALSE),
        gridGrobCoords(list("1"=gridCoords(1:2, 1:2)), "l"))
gtCheck(grobPoints(polylineGrob(1:2, 1:2, default.units="in", name="pl"),
                   closed=FALSE),
        gridGrobCoords(list("1"=gridCoords(1:2, 1:2)), "pl"))
gtCheck(grobPoints(polylineGrob(1:4, 1:4, id=rep(1:2, each=2),
                                default.units="in", name="pl"),
                   closed=FALSE),
        gridGrobCoords(list("1"=gridCoords(1:2, 1:2),
                            "2"=gridCoords(3:4, 3:4)), "pl"))
gtCheck(grobPoints(polygonGrob(1:2, 1:2, default.units="in", name="p"),
                   closed=TRUE),
        gridGrobCoords(list("1"=gridCoords(1:2, 1:2)), "p"))
gtCheck(grobPoints(polygonGrob(1:4, 1:4, id=rep(1:2, each=2),
                                default.units="in", name="p"),
                   closed=TRUE),
        gridGrobCoords(list("1"=gridCoords(1:2, 1:2),
                            "2"=gridCoords(3:4, 3:4)), "p"))
gtCheck(grobPoints(pathGrob(1:2, 1:2, default.units="in", name="p"),
                   closed=TRUE),
        gridGrobCoords(list("1"=gridCoords(1:2, 1:2)), "p", "winding"))
gtCheck(grobPoints(pathGrob(1:4, 1:4, id=rep(1:2, each=2),
                                default.units="in", name="p"),
                   closed=TRUE),
        gridGrobCoords(list("1"=gridCoords(1:2, 1:2),
                            "1"=gridCoords(3:4, 3:4)), "p", "winding"))
gtCheck(grobPoints(pathGrob(1:4, 1:4, pathId=rep(1:2, each=2),
                                default.units="in", name="p"),
                   closed=TRUE),
        gridGrobCoords(list("1"=gridCoords(1:2, 1:2),
                            "2"=gridCoords(3:4, 3:4)), "p", "winding"))
gtCheck(grobPoints(pathGrob(1:8, 1:8, id=rep(rep(1:2, each=2), 2),
                            pathId=rep(1:2, each=4),
                            default.units="in", name="p"),
                   closed=TRUE),
        gridGrobCoords(list("1"=gridCoords(1:2, 1:2),
                            "1"=gridCoords(3:4, 3:4),
                            "2"=gridCoords(5:6, 5:6),
                            "2"=gridCoords(7:8, 7:8)),
                       "p", "winding"))
gtCheck(grobPoints(segmentsGrob(1:2, 1, 3, 2, default.units="in", name="s"),
                   closed=FALSE),
        gridGrobCoords(list("1"=gridCoords(c(1, 3), 1:2),
                            "2"=gridCoords(c(2, 3), 1:2)), "s"))
x <- c(.5, .5, 1.5, 1.5)
y <- c(.5, 1.5, 1.5, .5)
r <- function(i) {
    rectGrob(1, 1, 1, 1, default.units="in", name=paste0("r", i))
}
r1 <- r(1)
r2 <- r(2)
r3 <- r(3)
gc <- function(i) {
    gridGrobCoords(list("1"=gridCoords(x, y)), paste0("r", i))
}
gc1 <- gc(1)
gc2 <- gc(2)
gc3 <- gc(3)
gtCheck(grobPoints(gTree(children=gList(r1), name="gt1"), closed=TRUE),
        gridGTreeCoords(list(gc1), "gt1"))
gtCheck(grobPoints(gTree(children=gList(r1, r2), name="gt1"), closed=TRUE),
        gridGTreeCoords(list(gc1, gc2),
                        "gt1"))
gtCheck(grobPoints(gTree(children=gList(r1,
                                        gTree(children=gList(r2), name="gt2"),
                                        r3),
                         name="gt1"), closed=TRUE),
        gridGTreeCoords(list(gc1,
                             gridGTreeCoords(list(gc2), "gt2"),
                             gc3),
                        "gt1"))
gtCheck(grobPoints(gTree(children=gList(gTree(children=gList(r1),
                                              name="gt2")),
                         name="gt1"),
                   closed=TRUE),
        gridGTreeCoords(list(gridGTreeCoords(list(gc1), "gt2")), "gt1"))
stopifnot(isEmptyCoords(grobPoints(gTree())))
stopifnot(isEmptyCoords(grobPoints(gTree(children=gList(segmentsGrob(0, 0:1,
                                                                     1, 1:0))),
                                   closed=TRUE)))

#################################
#################################
## Close PNG device
dev.off()
#################################
#################################
