## An environment not exported from namespace:grDevices used to
## pass .X11.Fonts to the X11 device.
.X11env <- new.env()

X11 <- function(display = "", width = 7, height = 7, pointsize = 12,
                gamma = 1, colortype = getOption("X11colortype"),
                maxcubesize = 256, bg = "transparent", canvas = "white",
                fonts = getOption("X11fonts"))
{

  if(display == "" && .Platform$GUI == "AQUA" && Sys.getenv("DISPLAY") == "")
      Sys.putenv(DISPLAY = ":0")
  .Internal(X11(display, width, height, pointsize, gamma, colortype,
                maxcubesize, bg, canvas, fonts, NA))
}

x11 <- X11

gnome <- function(display = "", width = 7, height = 7, pointsize = 12)
    .Defunct(package="grDevices")

## no Gnome <- .Alias(gnome)
GNOME <- gnome

####################
# X11 font database
####################

# Each font family has a name, plus a vector of 4 or 5 directories
# for font metric afm files
assign(".X11.Fonts", list(), envir = .X11env)

X11FontError <- function(errDesc)
    stop("invalid X11 font specification: ", errDesc)


# Check that the font has the correct structure and information
# Already checked that it had a name
checkX11Font <- function(font) {
  if (!is.character(font))
    X11FontError("must be a string")
  # Check it has the right format
  if (length(grep("(-[^-]+){14}", font)) > 0) {
    # Force the %s and %d substitution formats into the right spots
    font <- sub("((-[^-]+){2})(-[^-]+){2}((-[^-]+){2})(-[^-]+)((-[^-]+){7})",
                "\\1-%s-%s\\4-%d\\7", font)
  } else {
    X11FontError("incorrect format")
  }
  font
}

setX11Fonts <- function(fonts, fontNames) {
  fonts <- lapply(fonts, checkX11Font)
  fontDB <- get(".X11.Fonts", envir=.X11env)
  existingFonts <- fontNames %in% names(fontDB)
  if (sum(existingFonts) > 0)
    fontDB[fontNames[existingFonts]] <- fonts[existingFonts]
  if (sum(existingFonts) < length(fontNames))
    fontDB <- c(fontDB, fonts[!existingFonts])
  assign(".X11.Fonts", fontDB, envir=.X11env)
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
# of which must be valid X11 font descriptions and
# all of which must be named args)
X11Fonts <- function(...) {
  ndots <- length(fonts <- list(...))
  if (ndots == 0)
    get(".X11.Fonts", envir=.X11env)
  else {
    fontNames <- names(fonts)
    nnames <- length(fontNames)
    if (nnames == 0) {
      if (!all(sapply(fonts, is.character)))
        stop("invalid arguments in X11Fonts (must be font names)")
      else
        get(".X11.Fonts", envir=.X11env)[unlist(fonts)]
    } else {
      if (ndots != nnames)
        stop("invalid arguments in X11Fonts (need named args)")
      setX11Fonts(fonts, fontNames)
    }
  }
}

# Create a valid X11 font description
X11Font <- function(font) {
  checkX11Font(font)
}

X11Fonts(# Default Serif font is Times
         serif=X11Font("-*-times-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         # Default Sans Serif font is Helvetica
         sans=X11Font("-*-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         # Default Monospace font is Courier
         mono=X11Font("-*-courier-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         # Default Symbol font is Symbol
         symbol=X11Font("-*-symbol-%s-%s-*-*-%d-*-*-*-*-*-*-*"))
