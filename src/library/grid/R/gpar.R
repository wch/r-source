
# A "gpar" object is a list of graphics parameters
# A graphics parameter is a name-value pair

gpar <- function(...) {
  gp <- validGP(list(...))
  class(gp) <- "gpar"
  gp
}

is.gpar <- function(x) {
  inherits(x, "gpar")
}

validGP <- function(gpars) {
  # Check a (non-NULL) gpar is not of length 0
  check.length <- function(gparname) {
    if (length(gpars[[gparname]]) == 0)
      stop(paste("gpar element", gparname, "must not be length 0"))
  }
  # Check a gpar is numeric and not NULL
  numnotnull <- function(gparname) {
    if (!is.na(match(gparname, names(gpars)))) {
      if (is.null(gpars[[gparname]]))
        gpars[[gparname]] <<- NULL
      else {
        check.length(gparname)
        gpars[[gparname]] <<- as.numeric(gpars[[gparname]])
      }
    }
  }
  # fontsize, lineheight, cex, lwd should be numeric and not NULL
  numnotnull("fontsize")
  numnotnull("lineheight")
  numnotnull("cex")
  numnotnull("lwd")
  numnotnull("gamma")
  numnotnull("alpha")
  # col and fill are converted in C code
  # so is lty, BUT still want to check for NULL
  if (!is.na(match("lty", names(gpars)))) {
    if (is.null(gpars$lty))
      gpars$lty <- NULL
    else
      check.length("lty")
  }
  # font should be integer and not NULL
  if (!is.na(match("font", names(gpars)))) {
    if (is.null(gpars$font))
      gpars$font <- NULL
    else {
      check.length("font")
      gpars$font <- as.integer(gpars$font)
    }
  }
  # fontfamily should be character
  if (!is.na(match("fontfamily", names(gpars)))) {
    if (is.null(gpars$fontfamily))
      gpars$fontfamily <- NULL
    else {
      check.length("fontfamily")
      gpars$fontfamily <- as.character(gpars$fontfamily)
    }
  }
  # fontface can be character or integer;  map character to integer
  # store value in font
  # Illegal to specify both font and fontface
  if (!is.na(match("fontface", names(gpars)))) {
    if (!is.na(match("font", names(gpars))))
      stop("Must specify only one of font and fontface")
    if (is.null(gpars$fontface))
      gpars$font <- NULL
    else {
      check.length("fontface")
      if (is.numeric(gpars$fontface))
        gpars$font <- as.integer(gpars$fontface)
      else {
        temp.char <- as.character(gpars$fontface)
        temp.num <- 0
        for (i in 1:length(temp.char))
          temp.num[i] <- switch(temp.char[i],
                                plain=1,
                                italic=3,
                                oblique=3,
                                bold=2,
                                bold.italic=4,
                                symbol=5,
                                # These are Hershey variants
                                cyrillic=5,
                                cyrillic.oblique=6,
                                EUC=7)
        gpars$font <- as.integer(temp.num)
      }
    }
  }
  gpars
}

saved.pars <- function(pars) {
  list(prev=NULL, pars=pars)
}
push.saved.gpars <- function(gpars) {
  sp <- saved.pars(gpars)
  sp$prev <- grid.Call("L_getGPsaved")
  grid.Call("L_setGPsaved", sp)
}

pop.saved.gpars <- function() {
  grid.Call("L_setGPsaved", grid.Call("L_getGPsaved")$prev)
}

# possible gpar names
# The order must match the GP_* values in grid.h
.grid.gpar.names <- c("fill", "col", "gamma", "lty", "lwd", "cex",
                      "fontsize", "lineheight", "font", "fontfamily",
                      "alpha",
                      # Keep fontface at the end because it is never
                      # used in C code (it gets mapped to font)
                      "fontface")

# Set .grid.gpars to keep grid record of current settings
set.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("Argument must be a 'gpar' object")
  subset <- match(names(gp), .grid.gpar.names)
  cur.gpars <- grid.Call("L_getGPar")
  push.saved.gpars(cur.gpars[subset])
  temp <- cur.gpars
  temp[subset] <- gp
  # Do this as a .Call.graphics to get it onto the base display list
  grid.Call.graphics("L_setGPar", temp)
}

unset.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("Argument must be a 'gpar' object")
  # for debugging really
  subset <- match(names(gp), .grid.gpar.names)
  saved.gpars <- grid.Call("L_getGPsaved")
  if (length(subset) != length(saved.gpars$pars))
    stop(paste("Trying to reset", names(gp),
               "with", saved.gpars$pars))
  temp <- grid.Call("L_getGPar")
  temp[subset] <- saved.gpars$pars
  # Do this as a .Call.graphics to get it onto the base display list
  grid.Call.graphics("L_setGPar", temp)
  pop.saved.gpars()
}  

get.gpar <- function(gpar.name) {
  grid.Call("L_getGPar")[[gpar.name]]
}


