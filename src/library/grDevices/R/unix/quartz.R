.Quartzenv <- new.env()

quartz <- function (display = "", width = 5, height = 5, pointsize = 12, 
                    family="Helvetica", antialias = TRUE, autorefresh = TRUE){
  if (.Platform$GUI != "AQUA")
   warning("quartz() device interactivity reduced without an event loop manager")

    .Internal(Quartz(display, width, height, pointsize,family, antialias,autorefresh))
}


#########
# QUARTZ font database
# To map device-independent font to device-specific font
#########

# Each font family has only a name
assign(".Quartz.Fonts", list(), envir = .Quartzenv)

# Check that the font has the correct structure and information
checkQuartzFont <- function(font) {
  if (!is.character(font) || length(font) != 4)
    stop("Invalid Quartz font:  must be 4 strings")
  font
}

setQuartzFonts <- function(fonts, fontNames) {
  fonts <- lapply(fonts, checkQuartzFont)
  fontDB <- get(".Quartz.Fonts", envir=.Quartzenv)
  existingFonts <- fontNames %in% names(fontDB)
  if (sum(existingFonts) > 0)
    fontDB[fontNames[existingFonts]] <- fonts[existingFonts]
  if (sum(existingFonts) < length(fontNames))
    fontDB <- c(fontDB, fonts[!existingFonts])
  assign(".Quartz.Fonts", fontDB, envir=.Quartzenv)
}

printFont <- function(font) {
  paste(font, "\n", sep="")
}

printFonts <- function(fonts) {
  cat(paste(names(fonts), ": ", unlist(lapply(fonts, printFont)),
            sep="", collapse=""))
}

# If no arguments spec'ed, return entire font database
# If no named arguments spec'ed, all args should be font names
# to get info on from the database
# Else, must specify new fonts to enter into database (all
# of which must be valid PostScript font descriptions and
# all of which must be named args)
quartzFonts <- function(...) {
  ndots <- length(fonts <- list(...))
  if (ndots == 0)
    get(".Quartz.Fonts", envir=.Quartzenv)
  else {
    fontNames <- names(fonts)
    nnames <- length(fontNames)
    if (nnames == 0) {
      if (!all(sapply(fonts, is.character)))
        stop("Invalid arguments in quartzFonts (must be font names)")
      else
        get(".Quartz.Fonts", envir=.Quartzenv)[unlist(fonts)]
    } else {
      if (ndots != nnames)
        stop("Invalid arguments in quartzFonts (need NAMED args)")
      setQuartzFonts(fonts, fontNames)
    }
  }
}

# Create a valid quartz font description
quartzFont <- function(family) {
  checkQuartzFont(family)
}

quartzFonts(# Default Serif font is Times 
                serif=quartzFont(c("Times-Roman", "Times-Bold",
                  "Times-Italic", "Times-BoldItalic")),
                # Default Sans Serif font is Helvetica
                sans=quartzFont(c("Helvetica", "Helvetica-Bold",
                  "Helvetica-Italic", "Helvetica-BoldOblique")),
                # Default Monospace font is Courier
                mono=quartzFont(c("Courier", "Courier-Bold",
                  "Courier-Oblique", "Courier-BoldOblique")),
                # Default Symbol font is Symbol
                symbol=quartzFont(c("Symbol", "Symbol",
                  "Symbol", "Symbol")))
