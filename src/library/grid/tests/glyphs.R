
library(grDevices)
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

## NOTE that we make use of a font that has a free licence so
## that we can distribute the font along with 'grid' to standardize testing

## Values found from textshaping::shape_text(), but stored manually
## to avoid dependency on 'textshaping'
## 
## textshaping::shape_text("glyphs",
##                         path="Fonts/Montserrat/static/Montserrat-Medium.ttf")

## Do NOT use normalizePath() because it generates
## a path /home/staff/paul that ghostscript does NOT like

RobotoFont <- glyphFont(system.file("fonts", "Roboto", "Roboto-Medium.ttf",
                                    package="grDevices"),
                        0, "Roboto Medium", 400, "normal")
                        
RobotoInfo <- list(id = c(75, 80, 93, 84, 76, 87),
                   x = c(0, 6.796875, 9.859375, 15.703125,
                         22.453125, 29.109375),
                   y = rep(0, 6),
                   font = 1,
                   size = rep(12, 6),
                   fontList = glyphFontList(RobotoFont),
                   width = 35.29688,
                   height = 25.21875/2, ## divide by 2 cos of 'textshaping' bug 
                   hAnchor = glyphAnchor(0, "left"),
                   vAnchor = glyphAnchor(-(25.21875/2 - 11.14062), "bottom"))
Roboto <- do.call(glyphInfo, RobotoInfo)

MontserratFont <- glyphFont(system.file("fonts", "Montserrat", "static",
                                        "Montserrat-Medium.ttf",
                                        package="grDevices"),
                            0, "Montserrat Medium", 400, "normal")

MontserratInfo <- list(id = c(461, 499, 620, 556, 469, 567),
                       x = c(0.00000, 8.28125, 11.62500, 18.32812,
                             26.51562, 34.68750),
                       y = rep(0, 6),
                       font = 1,
                       size = rep(12, 6),
                       fontList = glyphFontList(MontserratFont),
                       width = glyphWidth(c(40.70312,
                                            40.70312 - 0.5 - 0.359375),
                                          label=c("width", "tight"),
                                          left=c("left", "leftBearing")),
                       height = glyphHeight(c(26.23438/2,
                                              26.23438/2 - 2.703125 - 0.609375),
                                            label=c("height", "tight"),
                                            bottom=c("bottom",
                                                     "bottomBearing")),
                       hAnchor = glyphAnchor(c(0, 0.5),
                                             label=c("left", "leftBearing")),
                       vAnchor = glyphAnchor(c(0,
                                               -(26.23438/2 - 11.60938),
                                               -(26.23438/2 - 11.60938 -
                                                 0.609375)),
                                             label=c("baseline", "bottom",
                                                     "bottomBearing")))
Montserrat <- do.call(glyphInfo, MontserratInfo)

## Set up "global" 'testGlyphInfo' so that pdfEmbeddedRecording() device
## can use it in call to embedGlyphs()
testGlyphInfo <- list()
testGlyph <- function(info, ...) {
    testGlyphInfo[[length(testGlyphInfo) + 1]] <<- info
    grid.glyph(testGlyphInfo[[length(testGlyphInfo)]], ...)
}
testGlyphGrob <- function(info, ...) {
    testGlyphInfo[[length(testGlyphInfo) + 1]] <<- info
    glyphGrob(testGlyphInfo[[length(testGlyphInfo)]], ...)
}

## glyphs
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(Montserrat)
HersheyLabel("Montserrat glyphs", y=.2)

grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(Roboto)
HersheyLabel("Roboto glyphs", y=.2)

## glyphs with NA/non-finite values
missingIDinfo <- MontserratInfo
missingIDinfo$id[1] <- NA
missingID <- do.call(glyphInfo, missingIDinfo)
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(missingID)
HersheyLabel("Missing glyph id ('g' missing)", y=.2)

missingXinfo <- MontserratInfo
missingXinfo$x[2] <- NA
missingX <- do.call(glyphInfo, missingXinfo)
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(missingX)
HersheyLabel("Missing glyph x ('l' missing)", y=.2)

missingYinfo <- MontserratInfo
missingYinfo$y[3] <- NA
missingY <- do.call(glyphInfo, missingYinfo)
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(missingY)
HersheyLabel("Missing glyph y ('y' missing)", y=.2)

## glyphs with font file non-existent (should produce warning)
## (output is unpredictable, but likely to be weird because the glyph ids
##  are unlikely to match the glyph ids in the substituted font)
nofile <- Roboto
nofile$fonts[[1]]$file <- "road/to/nowhere"
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(nofile)
HersheyLabel("Font file not found - output will be weird", y=.2)

## Manual hack of "RGlyphInfo" object
missingfile <- Roboto
missingfile$fonts[[1]]$file <- as.character(NA)
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(missingfile)
HersheyLabel("Font file not found - output will be weird", y=.2)

## glyphs with colour
colourInfo <- MontserratInfo
colourInfo$col <- rep("red", 6)
colour <- do.call(glyphInfo, colourInfo)
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(colour)
HersheyLabel("glyphs with colour (red)", y=.2)
    
## Missing colour is OK
missingColourInfo <- colourInfo
missingColourInfo$col[4] <- NA
missingColour <- do.call(glyphInfo, missingColourInfo)
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(missingColour)
HersheyLabel("glyphs with one colour missing (red -> black)", y=.2)

## glyphs with alignment
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
grid.segments(.5,0, .5,1, gp=gpar(col="grey"))
testGlyph(Montserrat, hjust="left", vjust="bottom")
HersheyLabel("left bottom justification", y=.2)

grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
grid.segments(.5,0, .5,1, gp=gpar(col="grey"))
testGlyph(Montserrat, hjust="left", vjust="baseline")
HersheyLabel("(left) baseline justification", y=.2)

grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
grid.segments(.5,0, .5,1, gp=gpar(col="grey"))
testGlyph(Montserrat, hjust=glyphJust(0, "tight"), vjust="baseline")
HersheyLabel("tight left (baseline) justification\n(tiny bit further left)",
             y=.2)

## rotated glyphs
grid.newpage()
pushViewport(viewport(angle=30))
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
grid.segments(.5,0, .5,1, gp=gpar(col="grey"))
testGlyph(Montserrat)
popViewport()
HersheyLabel("rotated glyphs", y=.2)

grid.newpage()
pushViewport(viewport(angle=30))
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
grid.segments(.5,0, .5,1, gp=gpar(col="grey"))
testGlyph(Montserrat, hjust="left", vjust="baseline")
popViewport()
HersheyLabel("rotated (left baseline justified) glyphs", y=.2)

## glyph x/y/width/height
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
grid.segments(.5,0, .5,1, gp=gpar(col="grey"))
testGlyph(Montserrat, name="glyph")
grid.segments(0, .5, grobX("glyph", 180), .5, gp=gpar(col="red"))
grid.segments(1, 1, grobX("glyph", 45), grobY("glyph", 45), gp=gpar(col="red"))
grid.rect(width=grobWidth("glyph"), height=grobHeight("glyph"),
          gp=gpar(fill=NA))
HersheyLabel("glyph x/y/width/height", y=.2)

grid.newpage()
pushViewport(viewport(angle=30))
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
grid.segments(.5,0, .5,1, gp=gpar(col="grey"))
testGlyph(Montserrat, hjust="left", vjust="baseline", name="glyph")
grid.segments(0, .5, grobX("glyph", 180), .5, gp=gpar(col="red"))
grid.segments(1, 1, grobX("glyph", 45), grobY("glyph", 45), gp=gpar(col="red"))
popViewport()
HersheyLabel("(rotated left baseline justified) glyph x/y/width/height", y=.2)

## glyphs in tiling pattern
grid.newpage()
pat <- pattern(gTree(children=gList(rectGrob(width=unit(2, "cm"),
                                             height=unit(1, "cm"),
                                             gp=gpar(fill="grey")),
                                    testGlyphGrob(Montserrat))),
               width=unit(2, "cm"), height=unit(1, "cm"),
               extend="repeat")
grid.circle(r=.3, gp=gpar(fill=pat))
HersheyLabel("glyphs as tiling pattern", y=.1)

## glyphs as clipping path
grid.newpage()
pushViewport(viewport(clip=testGlyphGrob(Montserrat)))
grid.segments(0, unit(.5, "npc") + unit(seq(-5, 5), "mm"),
              1, unit(.5, "npc") + unit(seq(-5, 5), "mm"),
              gp=gpar(col=c("red", "blue"), lwd=2))
popViewport()
HersheyLabel("glyphs as clipping path", y=.2)

## glyphs as mask
grid.newpage()
grid.segments(gp=gpar(col="red", lwd=20))
pushViewport(viewport(mask=testGlyphGrob(Montserrat,
                                         gp=gpar(col=rgb(0,0,0,.5)))))
grid.rect(gp=gpar(fill="black"))
popViewport()
HersheyLabel("glyphs as mask", y=.2)
                      
## glyphs in group
grid.newpage()
grid.group(testGlyphGrob(Montserrat), "xor", segmentsGrob(gp=gpar(lwd=20)))
HersheyLabel("glyphs in group (xor line)", y=.2)

## glyphs in (transformed) group 
grid.newpage()
grid.define(testGlyphGrob(Montserrat), name="glyphs")
pushViewport(viewport(width=2, height=4))
grid.use("glyphs")
popViewport()
HersheyLabel("glyphs in transformed group", y=.2)

## glyphs as path
grid.newpage()
grid.stroke(testGlyphGrob(Montserrat), gp=gpar(lwd=.5))
HersheyLabel("glyphs as (stroked) path", y=.2)

## multiple fonts
## printVals <- function(x) {
##     cat(paste0("c(", paste(temp$shape[[x]], collapse=", "), ")\n"))
## }
## 
## library(textshaping)
## temp <-
##     shape_text(c("hello ", "world!"),
##                id=1,
##                bold=c(FALSE, TRUE),
##                italic=c(FALSE, TRUE),
##                path=c("Fonts/Montserrat/static/Montserrat-Medium.ttf",
##                       "Fonts/Montserrat/static/Montserrat-BoldItalic.ttf"))
## printVals("index")
Montserrat2 <-
    glyphFontList(glyphFont(system.file("fonts", "Montserrat", "static",
                                        "Montserrat-Medium.ttf",
                                        package="grDevices"),
                            0, "Montserrat-Medium", 400, "normal"),
                  glyphFont(system.file("fonts", "Montserrat", "static",
                                        "Montserrat-BoldItalic.ttf",
                                        package="grDevices"),
                            0, "Montserrat-BoldItalic", 700, "italic"))
MontserratInfo2 <-
    list(id = c(469, 434, 499, 499, 521, 1642, 614, 521, 559, 499, 427, 1606),
         x = c(0, 8.171875, 15.515625, 18.859375, 22.203125, 29.828125,
               33.0625, 44.171875, 52.015625, 57.125, 60.734375, 69.03125),
         y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         font = rep(1:2, each=6),
         fontList = Montserrat2,
         size = rep(12, 12),
         width = glyphWidth(c(72.5,
                              72.5 - 1.09375 - -0.421875),
                            label=c("width", "tight"),
                            left=c("left", "leftBearing")),
         height = glyphHeight(c(26.23438/2,
                                26.23438/2 - 2.703125 - 2.921875),
                              label=c("height", "tight"),
                              bottom=c("bottom",
                                       "bottomBearing")),
         hAnchor = glyphAnchor(c(0, 1.09375),
                               label=c("left", "leftBearing")),
         vAnchor = glyphAnchor(c(0,
                                 -(26.23438/2 - 11.60938),
                                 -(26.23438/2 - 11.60938 -
                                   2.921875)),
                               label=c("baseline", "bottom",
                                       "bottomBearing")))
Montserrat2 <- do.call(glyphInfo, MontserratInfo2)

grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
testGlyph(Montserrat2)
HersheyLabel("Montserrat glyphs (mixed style)", y=.2)

## Normal text plus glyphs 
grid.newpage()
grid.segments(0,.5,1,.5, gp=gpar(col="grey"))
grid.text("test", y=3/4)
testGlyph(Montserrat)
HersheyLabel("Montserrat glyphs plus normal text", y=.2)
