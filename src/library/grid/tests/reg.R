library(grid)

# Unit conversions
stopifnot(round(convertX(unit(1, "inches"), "inches", valueOnly=TRUE) - 1,
                digits=5) == 0)
stopifnot(round(convertX(unit(2.54, "cm"), "inches", valueOnly=TRUE) - 1,
                digits=5) == 0)
stopifnot(round(convertX(unit(25.4, "mm"), "inches", valueOnly=TRUE) - 1,
                digits=5) == 0)
stopifnot(round(convertX(unit(72.27, "points"), "inches", valueOnly=TRUE) - 1,
                digits=5) == 0)
stopifnot(round(convertX(unit(1/12*72.27, "picas"), "inches",
                         valueOnly=TRUE) - 1, digits=5) == 0)
stopifnot(round(convertX(unit(72, "bigpts"), "inches", valueOnly=TRUE) - 1,
                digits=5) == 0)
stopifnot(round(convertX(unit(1157/1238*72.27, "dida"), "inches",
                         valueOnly=TRUE) - 1, digits=5) == 0)
stopifnot(round(convertX(unit(1/12*1157/1238*72.27, "cicero"), "inches",
                         valueOnly=TRUE) - 1, digits=5) == 0)
stopifnot(round(convertX(unit(65536*72.27, "scaledpts"), "inches",
                         valueOnly=TRUE) - 1, digits=5) == 0)
stopifnot(round(convertX(unit(1/2.54, "inches"), "cm", valueOnly=TRUE) - 1,
                digits=5) == 0)
stopifnot(round(convertX(unit(1/25.4, "inches"), "mm", valueOnly=TRUE) - 1,
                digits=5) == 0)
stopifnot(round(convertX(unit(1/72.27, "inches"), "points",
                         valueOnly=TRUE) - 1, digits=5) == 0)
stopifnot(round(convertX(unit(1/(1/12*72.27), "inches"), "picas",
                         valueOnly=TRUE) - 1, digits=5) == 0)
stopifnot(round(convertX(unit(1/72, "inches"), "bigpts", valueOnly=TRUE) - 1,
                digits=5) == 0)
stopifnot(round(convertX(unit(1/(1157/1238*72.27), "inches"), "dida",
                         valueOnly=TRUE) - 1, digits=5) == 0)
stopifnot(round(convertX(unit(1/(1/12*1157/1238*72.27), "inches"), "cicero",
                         valueOnly=TRUE) - 1, digits=5) == 0)
stopifnot(round(convertX(unit(1/(65536*72.27), "inches"), "scaledpts",
                         valueOnly=TRUE) - 1, digits=5) == 0)

pushViewport(viewport(width=unit(1, "inches"),
                       height=unit(2, "inches"),
                       xscale=c(0, 1),
                       yscale=c(1, 3)))
  ## Location versus dimension
stopifnot(round(convertY(unit(2, "native"), "inches", valueOnly=TRUE) -
                1, digits=5) == 0)
stopifnot(round(convertHeight(unit(2, "native"), "inches", valueOnly=TRUE) -
                2, digits=5) == 0)
  ## From "x" to "y" (the conversion is via "inches")
stopifnot(round(convertUnit(unit(1, "native"), "native",
                            axisFrom="x", axisTo="y", valueOnly=TRUE) -
                2, digits=5) == 0)
  ## Convert several values at once
stopifnot(all(round(convertX(unit(c(0.5, 2.54), c("npc", "cm")),
                             c("inches", "native"), valueOnly=TRUE) -
                    c(0.5, 1), digits=5) == 0))
popViewport()

# packing a frame inside a frame
fg <- frameGrob()
fg <- packGrob(fg, textGrob("Hi there"))

fg2 <- frameGrob()
fg2 <- packGrob(fg2, fg)
fg2 <- packGrob(fg2, rectGrob(), side="bottom")
fg2 <- packGrob(fg2, rectGrob(height=unit(1, "inches")), side="top")

stopifnot(convertHeight(fg2$framevp$layout$heights, "inches",
                        valueOnly=TRUE)[2] < 1)

