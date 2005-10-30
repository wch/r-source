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
	 cidfamily = "default",
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
	stop(gettextf("invalid arguments in '%s'  (need named args)",
                      deparse(sys.call(sys.parent()))), domain = NA)
    if(!is.character(name.opt))
	stop("'name.opt' must be character, name of an existing list")
    if(reset) {
	if(exists(name.opt, envir=envir, inherits=FALSE)) {
	    if(length(utils::find(name.opt)) > 1)
		rm(list=name.opt, envir=envir)
##-	    else
##-		stop(paste("Cannot reset '", name.opt,
##-			"'  since it exists only once in search()!\n", sep=""))

	} else stop(gettextf("cannot reset non-existent '%s'", name.opt),
                    domain = NA)
    }
    old <- get(name.opt, envir=envir)
    if(!is.list(old))
	stop("invalid options in ", sQuote(name.opt))
    oldnames <- names(old)
    if(lnew > 0) {
	matches <- pmatch(newnames, oldnames)
	if(any(is.na(matches)))
	    stop(sprintf(ngettext(as.integer(sum(is.na(matches))),
                                 "invalid argument name %s in '%s'",
                                 "invalid argument names %s in '%s'"),
                         paste(sQuote(newnames[is.na(matches)]),
                               collapse=", "),
                         deparse(sys.call(sys.parent()))),
                 domain = NA)
##-- This does not happen: ambiguities are plain "NA" here:
##-	else if(any(matches==0))
##-	    stop(paste("ambiguous argument name(s) '",
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
		    warning(paste(sQuote(paste(fn,"(",names(prev[ii]),")",
                                               sep="")),
                                  collapse=" and "), " ",
                            ngettext(as.integer(sum(ii)),
                                     "differs between new and previous",
                                     "differ between new and previous"),
                            if(any(do.keep)) {
                                paste("\n\t ==> ",
                                      gettext("NOT changing "),
                                      paste(sQuote(names(prev[do.keep])),
                                            collapse=" & "),
                                      sep = "")},
                            domain = NA, call. = FALSE)
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

guess_encoding <- function()
{
    switch(.Platform$OS.type,
           "windows" = {
               switch(utils::localeToCharset()[1],
                      "ISO8859-2" = "CP1250.enc",
                      "ISO8859-7" = "CP1253.enc", # Greek
                      "ISO8859-13" = "CP1257.enc",
                      "CP1251" = "CP1251.enc", # Cyrillic
                      "WinAnsi.enc")
           },
       { lc <- localeToCharset()
         if(length(lc) == 1)
             switch(lc,
                    "ISO8859-1" = "ISOLatin1.enc",
                    "ISO8859-2" = "ISOLatin2.enc",
                    "ISO8859-5" = "Cyrillic.enc",
                    "ISO8859-7" = "Greek.enc",
                    "ISO8859-13" = "ISOLatin7.enc",
                    "ISO8859-15" = "ISOLatin9.enc",
                    "KOI8-R" = "KOI8-R.enc",
                    "KOI8-U" = "KOI8-U.enc",
                    "ISOLatin1.enc")
         else if(lc[1] == "UTF-8" && capabilities("iconv"))
             switch(lc[2],
                    "ISO8859-1" = "ISOLatin1.enc", # what about Euro?
                    "ISO8859-2" = "ISOLatin2.enc",
                    "ISO8859-5" = "Cyrillic.enc",
                    "ISO8859-7" = "Greek.enc",
                    "ISO8859-13" = "ISOLatin7.enc",
                    "ISOLatin1.enc")
         else "ISOLatin1.enc"})
}

guess_cidfamily <- function()
{
    switch(toupper(gsub("^[-\s 0-9a-zA-Z]*_",
                        "",
                        gsub("\.[-_0-9a-zA-Z]*$",
                             "",
                             Sys.getlocale("LC_CTYPE")))),
           "JAPAN"                      = "Japan1",
           "JP"                         = "Japan1",
           "KOREA"                      = "Korea1",
           "KR"                         = "Korea1",
           "TAIWAN"                     = "CNS1",
           "TW"                         = "CNS1",
           "MACAU S.A.R."               = "CNS1",
           "HONG KONG S.A.R."           = "CNS1",
           "HK"                         = "CNS1",
           "PEOPLE'S REPUBLIC OF CHINA" = "GB1",
           "CN"                         = "GB1",
           "SINGAPORE"                  = "GB1",
           "SG"                         = "GB1",
           "")
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
            family <- c(family, "Symbol.afm")
        } else {
            # If family has been defined as device-independent
            # R graphics family (i.e., it can be found in postscriptFonts)
            # then map to postscript font family
            if (length(family) == 1) {
                psFamily <- postscriptFonts(family)[[1]]
                if (!is.null(psFamily)) family <- psFamily$family
            }
        }
        old$family <- family
    }
    if(is.null(old$encoding) || old$encoding  == "default")
        old$encoding <- guess_encoding()
    # CID Font
    if(is.null(old$cidfamily) || old$cidfamily  == "default")
        old$cidfamily <- guess_cidfamily()

    .External("PostScript",
              file, old$paper, old$family, old$encoding, old$cidfamily, old$bg, old$fg,
              old$width, old$height, old$horizontal, old$pointsize,
              old$onefile, old$pagecentre, old$print.it, old$command,
              title, fonts, PACKAGE = "grDevices")
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
              old$onefile, old$pagecentre, PACKAGE = "grDevices")
    invisible()
}

pdf <- function (file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"),
                 width = 6, height = 6, onefile = TRUE, family,
                 title = "R Graphics Output", fonts = NULL, version="1.1",
                 paper = "special", ...)
{
    # paper explicit because "special" (not "default") by default
    new <- list(onefile=onefile, paper=paper, ...)# eval
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = FALSE, assign.opt = FALSE)
    if(is.null(old$encoding) || old$encoding  == "default")
        old$encoding <- guess_encoding()
    ## handle family separately as length can be 1, 4, or 5
    if(!missing(family)) {
        if(length(family) == 4) {
            family <- c(family, "Symbol.afm")
        } else {
        # If family has been defined as device-independent
        # R graphics family (i.e., it can be found in postscriptFonts)
        # then map to postscript font family
            if (length(family) == 1) {
                psFamily <- postscriptFonts(family)[[1]]
                if (!is.null(psFamily)) family <- psFamily$family
            }
        }
        old$family <- family
    }
    # CID Font
    if(is.null(old$cidfamily) || old$cidfamily  == "default")
        old$cidfamily <- guess_cidfamily()
    # Extract version
    versions <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6")
    if (version %in% versions)
        version <- as.integer(strsplit(version, "[.]")[[1]])
    else
        stop("invalid PDF version")
    .External("PDF",
              file, old$paper, old$family, old$encoding, old$cidfamily, old$bg, old$fg,
              width, height, old$pointsize, old$onefile, old$pagecentre, title,
              fonts, version[1], version[2], PACKAGE = "grDevices")
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
    stop("invalid ", errDesc, " in PostScript font specification")
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
        font$metrics <- c(font$metrics, "Symbol.afm")
    if (is.null(font$encoding) || !is.character(font$encoding))
        psFontError("font encoding")
    font
}

checkFontInUse <- function(names) {
    for (i in names)
        if (.Call("Type1FontInUse", i, PACKAGE = "grDevices"))
            stop("font" , i, " already in use")
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
                stop("invalid arguments in postscriptFonts (must be font names)")
            else
                get(".PostScript.Fonts", envir=.PSenv)[unlist(fonts)]
        } else {
            if (ndots != nnames)
                stop("invalid arguments in postscriptFonts (need named args)")
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
                  c("Times-Roman.afm", "Times-Bold.afm",
                    "Times-Italic.afm", "Times-BoldItalic.afm",
                    "Symbol.afm")),
                # Default Sans Serif font is Helvetica
                sans=postscriptFont("Helvetica",
                  c("Helvetica.afm", "Helvetica-Bold.afm",
                    "Helvetica-Oblique.afm", "Helvetica-BoldOblique.afm",
                    "Symbol.afm")),
                # Default Monospace font is Courier
                mono=postscriptFont("Courier",
                  c("Courier.afm", "Courier-Bold.afm",
                    "Courier-Oblique.afm", "Courier-BoldOblique.afm",
                    "Symbol.afm")),
                # Default Symbol font is Symbol
                symbol=postscriptFont("Symbol",
                  c("Symbol.afm", "Symbol.afm", "Symbol.afm", "Symbol.afm",
                    "Symbol.afm"), encoding="AdobeSym.enc"),
                # Remainder are standard Adobe fonts that
                # should be present on PostScript devices
                AvantGarde=postscriptFont("AvantGarde",
                  c("agw_____.afm", "agd_____.afm",
                    "agwo____.afm", "agdo____.afm",
                    "Symbol.afm")),
                Bookman=postscriptFont("Bookman",
                  c("bkl_____.afm", "bkd_____.afm",
                    "bkli____.afm", "bkdi____.afm",
                    "Symbol.afm")),
                Courier=postscriptFont("Courier",
                  c("Courier.afm", "Courier-Bold.afm",
                    "Courier-Oblique.afm", "Courier-BoldOblique.afm",
                    "Symbol.afm")),
                Helvetica=postscriptFont("Helvetica",
                  c("Helvetica.afm", "Helvetica-Bold.afm",
                    "Helvetica-Oblique.afm", "Helvetica-BoldOblique.afm",
                    "Symbol.afm")),
                HelveticaNarrow=postscriptFont("Helvetica-Narrow",
                  c("hvn_____.afm", "hvnb____.afm",
                    "hvno____.afm", "hvnbo___.afm",
                    "Symbol.afm")),
                NewCenturySchoolbook=postscriptFont("NewCenturySchoolbook",
                  c("ncr_____.afm", "ncb_____.afm",
                    "nci_____.afm", "ncbi____.afm",
                    "Symbol.afm")),
                Palatino=postscriptFont("Palatino",
                  c("por_____.afm", "pob_____.afm",
                    "poi_____.afm", "pobi____.afm",
                    "Symbol.afm")),
                Times=postscriptFont("Times",
                  c("Times-Roman.afm", "Times-Bold.afm",
                    "Times-Italic.afm", "Times-BoldItalic.afm",
                    "Symbol.afm")),
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
                    "CM_symbol_10.afm"), encoding = "TeXtext.enc"),
                 ComputerModernItalic=postscriptFont("ComputerModernItalic",
                  c("CM_regular_10.afm", "CM_boldx_10.afm", "cmti10.afm",
                    "cmbxti10.afm", "CM_symbol_10.afm"),
                 encoding = "TeXtext.enc")
               )
