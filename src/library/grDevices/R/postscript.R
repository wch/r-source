## An environment not exported from namespace:graphics used to
## pass .PostScript.Options to the windows() device for use in its menus.
## and also to hide the variable.
.PSenv <- new.env()

assign(".PostScript.Options",
    list(paper	= "default",
	 horizontal = TRUE,
	 width	= 0,
	 height = 0,
	 family = "Helvetica",
	 encoding = "default",
	 pointsize  = 12,
	 bg	= "transparent",
	 fg	= "black",
	 onefile    = TRUE,
	 print.it   = FALSE,
	 append	    = FALSE,
	 pagecentre = TRUE,
	 command    = "default"), envir = .PSenv)

check.options <-
    function(new, name.opt, reset = FALSE, assign.opt = FALSE,
	     envir = .GlobalEnv, check.attributes = c("mode", "length"),
	     override.check = FALSE)
{
    lnew <- length(new)
    if(lnew != length(newnames <- names(new)))
	stop(paste("invalid arguments in \"",
		   deparse(sys.call(sys.parent())),
		   "\" (need NAMED args)", sep=""))
    if(!is.character(name.opt))
	stop("'name.opt' must be character, name of an existing list")
    if(reset) {
	if(exists(name.opt, envir=envir, inherits=FALSE)) {
	    if(length(utils::find(name.opt)) > 1)
		rm(list=name.opt, envir=envir)
##-	    else
##-		stop(paste("Cannot reset '", name.opt,
##-			"'  since it exists only once in search()!\n", sep=""))

	} else stop(paste("Cannot reset non-existing '", name.opt, "'", sep=""))
    }
    old <- get(name.opt, envir=envir)
    if(!is.list(old))
	stop(paste("invalid options in `",name.opt,"'",sep=""))
    oldnames <- names(old)
    if(lnew > 0) {
	matches <- pmatch(newnames, oldnames)
	if(any(is.na(matches)))
	    stop(paste("invalid argument name(s) `",
		       paste(newnames[is.na(matches)], collapse=", "),
		       "' in \"", deparse(sys.call(sys.parent())),"\"",sep=""))
##-- This does not happen: ambiguities are plain "NA" here:
##-	else if(any(matches==0))
##-	    stop(paste("ambiguous argument name(s) `",
##-			   paste(newnames[matches == 0], collapse=", "),
##-			   "' in \"", deparse(sys.call(sys.parent())),"\"",sep=""))
	else { #- match(es) found:  substitute if appropriate
	    i.match <- oldnames[matches]
	    prev <- old[i.match]
	    doubt <- rep.int(FALSE, length(prev))
	    for(fn in check.attributes)
		if(any(ii <- sapply(prev, fn) != sapply(new, fn))) {
		    doubt <- doubt | ii
		    do.keep <- ii & !override.check
		    warning(paste(paste(paste("`",fn,"(",names(prev[ii]),")'",
					      sep=""),
					collapse=" and "),
				  " differ", if(sum(ii)==1) "s",
				  " between new and previous!",
				  if(any(do.keep))
				  paste("\n\t ==> NOT changing ",
					paste(paste("`",names(prev[do.keep]),
						    "'", sep=""),
					      collapse=" & "),
					collapse = ""),
				  sep=""))
		}
	    names(new) <- NULL
	    if(any(doubt)) {
		ii <- !doubt | override.check
		old[i.match[ii]] <- new[ii]
	    } else old[i.match] <- new

	}
	if(assign.opt) assign(name.opt, old, envir=envir)
    }
    old
}

ps.options <- function(..., reset=FALSE, override.check= FALSE)
{
    l... <- length(new <- list(...))
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = as.logical(reset), assign.opt = l... > 0,
			 override.check= override.check)
    if(reset || l... > 0) invisible(old)
    else old
}

##--> source in devPS.c :

postscript <- function (file = ifelse(onefile,"Rplots.ps", "Rplot%03d.ps"),
                        onefile = TRUE, family,
                        title = "R Graphics Output",
                        fonts = NULL, ...)
{
    new <- list(onefile=onefile, ...)# eval
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = FALSE, assign.opt = FALSE)

    if(is.null(old$command) || old$command == "default")
        old$command <- if(!is.null(cmd <- getOption("printcmd"))) cmd else ""
    ## handle family separately as length can be 1, 4, or 5
    if(!missing(family)) {
        if(length(family) == 4) {
            family <- c(family, "sy______.afm")
        } else {
            # If family has been defined as device-independent
            # R graphics family (i.e., it can be found in postscriptFonts)
            # then map to postscript font family
            if (length(family) == 1) {
                psFamily <- postscriptFonts(family)[[1]]
                if (!is.null(psFamily))
                    family <- psFamily$family
            }
        }
        old$family <- family
    }
    if(is.null(old$encoding) || old$encoding  == "default")
        old$encoding <- switch(.Platform$OS.type,
                               "windows" = "WinAnsi.enc",
                               "ISOLatin1.enc")
    .External("PostScript",
              file, old$paper, old$family, old$encoding, old$bg, old$fg,
              old$width, old$height, old$horizontal, old$pointsize,
              old$onefile, old$pagecentre, old$print.it, old$command,
              title, fonts, PACKAGE = "base")
    # if .ps.prolog is searched for and fails, NULL got returned.
    invisible()
}

xfig <- function (file = ifelse(onefile,"Rplots.fig", "Rplot%03d.fig"),
                  onefile = FALSE, ...)
{
    new <- list(onefile=onefile, ...)# eval
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = FALSE, assign.opt = FALSE)

    .External("XFig",
              file, old$paper, old$family, old$bg, old$fg,
              old$width, old$height, old$horizontal, old$pointsize,
              old$onefile, old$pagecentre, PACKAGE = "base")
    invisible()
}

pdf <- function (file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"),
                 width = 6, height = 6, onefile = TRUE, family,
                 title = "R Graphics Output", fonts = NULL, version="1.1", ...)
{
    new <- list(onefile=onefile, ...)# eval
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = FALSE, assign.opt = FALSE)
    if(is.null(old$encoding) || old$encoding  == "default")
        old$encoding <- switch(.Platform$OS.type,
                               "windows" = "WinAnsi.enc",
                               "ISOLatin1.enc")
    if(!missing(family)) {
        if (!is.character(family) || length(family) != 1)
            stop("Invalid family argument")
        # If family has been defined as device-independent
        # R graphics family (i.e., it can be found in postscriptFonts)
        # then map to postscript font family
        else {
            psFamily <- postscriptFonts(family)[[1]]
            if (!is.null(psFamily))
                family <- psFamily$family
        }
        old$family <- family
    }
    # Extract version
    versions <- c("1.1", "1.2", "1.3", "1.4")
    if (version %in% versions)
        version <- as.integer(strsplit(version, "[.]")[[1]])
    else
        stop("Invalid PDF version")
    .External("PDF",
              file, old$family, old$encoding, old$bg, old$fg,
              width, height, old$pointsize, old$onefile, title,
              fonts, version[1], version[2], PACKAGE = "base")
    invisible()
}

.ps.prolog <- c(
"/gs  { gsave } def",
"/gr  { grestore } def",
"/ep  { showpage gr gr } def",
"/m   { moveto } def",
"/l  { rlineto } def",
"/np  { newpath } def",
"/cp  { closepath } def",
"/f   { fill } def",
"/o   { stroke } def",
"/c   { newpath 0 360 arc } def",
"/r   { 4 2 roll moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch rlineto closepath } def",
"/p1  { stroke } def",
"/p2  { gsave bg setrgbcolor fill grestore newpath } def",
"/p3  { gsave bg setrgbcolor fill grestore stroke } def",
"/t   { 6 -2 roll moveto gsave rotate",
"       ps mul neg 0 2 1 roll rmoveto",
"       1 index stringwidth pop",
"       mul neg 0 rmoveto show grestore } def",
"/cl  { grestore gsave newpath 3 index 3 index moveto 1 index",
"       4 -1 roll lineto  exch 1 index lineto lineto",
"       closepath clip newpath } def",
"/rgb { setrgbcolor } def",
"/s   { scalefont setfont } def")
# "/R   { /Font1 findfont } def",
# "/B   { /Font2 findfont } def",
# "/I   { /Font3 findfont } def",
# "/BI  { /Font4 findfont } def",
# "/S   { /Font5 findfont } def",
# "1 setlinecap 1 setlinejoin")

####################
# PostScript font database
####################

# Each font family has a name, plus a vector of 4 or 5 directories
# for font metric afm files
assign(".PostScript.Fonts", list(), envir = .PSenv)

psFontError <- function(errDesc) {
    stop(paste("Invalid", errDesc, "in PostScript font specification"))
}

# Check that the font has the correct structure and information
# Already checked that it had a name
checkPSFont <- function(font) {
    if (is.null(font$family) || !is.character(font$family))
        psFontError("font family name")
    if (is.null(font$metrics) || !is.character(font$metrics) ||
        length(font$metrics) < 4)
        psFontError("font metric information")
        ## Add default symbol font metric if none provided
    if (length(font$metrics) == 4)
        font$metrics <- c(font$metrics, "sy______.afm")
    if (is.null(font$encoding) || !is.character(font$encoding))
        psFontError("font encoding")
    font
}

checkFontInUse <- function(names) {
    for (i in names)
        if (.Call("Type1FontInUse", i, PACKAGE = "base"))
            stop(paste("Font", i, "already in use"))
}

setPSFonts <- function(fonts, fontNames) {
    fonts <- lapply(fonts, checkPSFont)
    fontDB <- get(".PostScript.Fonts", envir=.PSenv)
    existingFonts <- fontNames %in% names(fontDB)
    if (sum(existingFonts) > 0) {
        checkFontInUse(fontNames[existingFonts])
        fontDB[fontNames[existingFonts]] <- fonts[existingFonts]
    }
    if (sum(existingFonts) < length(fontNames))
        fontDB <- c(fontDB, fonts[!existingFonts])
    assign(".PostScript.Fonts", fontDB, envir=.PSenv)
}

printFont <- function(font) {
    paste(font$family, "\n    (", paste(font$metrics, collapse=" "), ")\n",
          sep="")
}

printFonts <- function(fonts) {
    cat(paste(names(fonts), ": ", unlist(lapply(fonts, printFont)),
              sep="", collapse=""))
}

# If no arguments specified, return entire font database
# If no named arguments specified, all args should be font names
# on which to get info from the database
# Else, must specify new fonts to enter into database (all
# of which must be valid PostScript font descriptions and
# all of which must be named args)
postscriptFonts <- function(...) {
    ndots <- length(fonts <- list(...))
    if (ndots == 0)
        get(".PostScript.Fonts", envir=.PSenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0) {
            if (!all(sapply(fonts, is.character)))
                stop("Invalid arguments in postscriptFonts (must be font names)")
            else
                get(".PostScript.Fonts", envir=.PSenv)[unlist(fonts)]
        } else {
            if (ndots != nnames)
                stop("Invalid arguments in postscriptFonts (need NAMED args)")
            setPSFonts(fonts, fontNames)
        }
    }
}

# Create a valid postscript font description
postscriptFont <- function(family, metrics, encoding="default") {
    checkPSFont(list(family=family, metrics=metrics, encoding=encoding))
}

postscriptFonts(# Default Serif font is Times
                serif=postscriptFont("Times",
                  c("tir_____.afm", "tib_____.afm",
                    "tii_____.afm", "tibi____.afm",
                    "sy______.afm")),
                # Default Sans Serif font is Helvetica
                sans=postscriptFont("Helvetica",
                  c("hv______.afm", "hvb_____.afm",
                    "hvo_____.afm", "hvbo____.afm",
                    "sy______.afm")),
                # Default Monospace font is Courier
                mono=postscriptFont("Courier",
                  c("com_____.afm", "cob_____.afm",
                    "coo_____.afm", "cobo____.afm",
                    "sy______.afm")),
                # Default Symbol font is Symbol
                symbol=postscriptFont("Symbol",
                  c("sy______.afm", "sy______.afm",
                    "sy______.afm", "sy______.afm",
                    "sy______.afm"),
                  encoding="AdobeSym.enc"),
                # Remainder are standard Adobe fonts that
                # should be present on PostScript devices
                AvantGarde=postscriptFont("AvantGarde",
                  c("agw_____.afm", "agd_____.afm",
                    "agwo____.afm", "agdo____.afm",
                    "sy______.afm")),
                Bookman=postscriptFont("Bookman",
                  c("bkl_____.afm", "bkd_____.afm",
                    "bkli____.afm", "bkdi____.afm",
                    "sy______.afm")),
                Courier=postscriptFont("Courier",
                  c("com_____.afm", "cob_____.afm",
                    "coo_____.afm", "cobo____.afm",
                    "sy______.afm")),
                Helvetica=postscriptFont("Helvetica",
                  c("hv______.afm", "hvb_____.afm",
                    "hvo_____.afm", "hvbo____.afm",
                    "sy______.afm")),
                HelveticaNarrow=postscriptFont("Helvetica-Narrow",
                  c("hvn_____.afm", "hvnb____.afm",
                    "hvno____.afm", "hvnbo___.afm",
                    "sy______.afm")),
                NewCenturySchoolbook=postscriptFont("NewCenturySchoolbook",
                  c("ncr_____.afm", "ncb_____.afm",
                    "nci_____.afm", "ncbi____.afm",
                    "sy______.afm")),
                Palatino=postscriptFont("Palatino",
                  c("por_____.afm", "pob_____.afm",
                    "poi_____.afm", "pobi____.afm",
                    "sy______.afm")),
                Times=postscriptFont("Times",
                  c("tir_____.afm", "tib_____.afm",
                    "tii_____.afm", "tibi____.afm",
                    "sy______.afm")),
                # URW equivalents
                URWGothic=postscriptFont("URWGothic",
                  c("a010013l.afm", "a010015l.afm",
                    "a010033l.afm", "a010035l.afm",
                    "s050000l.afm")),
                URWBookman=postscriptFont("URWBookman",
                  c("b018012l.afm", "b018015l.afm",
                    "b018032l.afm", "b018035l.afm",
                    "s050000l.afm")),
                NimbusMon=postscriptFont("NimbusMon",
                  c("n022003l.afm", "n022004l.afm",
                    "n022023l.afm", "n022024l.afm",
                    "s050000l.afm")),
                NimbusSan=postscriptFont("NimbusSan",
                  c("n019003l.afm", "n019004l.afm",
                    "n019023l.afm", "n019024l.afm",
                    "s050000l.afm")),
                URWHelvetica=postscriptFont("URWHelvetica",
                  c("n019003l.afm", "n019004l.afm",
                    "n019023l.afm", "n019024l.afm",
                    "s050000l.afm")),
                NimbusSanCond=postscriptFont("NimbusSanCond",
                  c("n019043l.afm", "n019044l.afm",
                    "n019063l.afm", "n019064l.afm",
                    "s050000l.afm")),
                CenturySch=postscriptFont("CenturySch",
                  c("c059013l.afm", "c059016l.afm",
                    "c059033l.afm", "c059036l.afm",
                    "s050000l.afm")),
                URWPalladio=postscriptFont("URWPalladio",
                  c("p052003l.afm", "p052004l.afm",
                    "p052023l.afm", "p052024l.afm",
                    "s050000l.afm")),
                NimbusRom=postscriptFont("NimbusRom",
                  c("n021003l.afm", "n021004l.afm",
                    "n021023l.afm", "n021024l.afm",
                    "s050000l.afm")),
                URWTimes=postscriptFont("URWTimes",
                  c("n021003l.afm", "n021004l.afm",
                    "n021023l.afm", "n021024l.afm",
                    "s050000l.afm")),
                # Computer Modern as recoded by Brian D'Urso
                ComputerModern=postscriptFont("ComputerModern",
                  c("CM_regular_10.afm", "CM_boldx_10.afm",
                    "CM_italic_10.afm", "CM_boldx_italic_10.afm",
                    "CM_symbol_10.afm")))
