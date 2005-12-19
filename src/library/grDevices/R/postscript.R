## An environment not exported from namespace:graphics used to
## pass .PostScript.Options to the windows() device for use in its menus.
## and also to hide the variable.
.PSenv <- new.env()

check.options <-
    function(new, name.opt, reset = FALSE, assign.opt = FALSE,
	     envir = .GlobalEnv, check.attributes = c("mode", "length"),
	     override.check = FALSE)
{
    lnew <- length(new)
    if(lnew != length(newnames <- names(new)))
	stop(gettextf("invalid arguments in '%s' (need named args)",
                      deparse(sys.call(sys.parent()))), domain = NA)
    if(!is.character(name.opt))
	stop("'name.opt' must be character, name of an existing list")
    if(reset) {
	if(exists(name.opt, envir=envir, inherits=FALSE)) {
	    if(length(utils::find(name.opt)) > 1)
		rm(list=name.opt, envir=envir)

	} else stop(gettextf("cannot reset non-existent '%s'", name.opt),
                    domain = NA)
    }
    old <- get(name.opt, envir=envir)
    if(!is.list(old))
	stop(gettextf("invalid options in '%s'", name.opt), domain = NA)
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

guessEncoding <- function(family)
{
    # Three special families have special encodings, regardless of locale
    if (!missing(family) &&
        family %in% c("symbol", "ComputerModern", "ComputerModernItalic")) {
        switch(family,
               "symbol" = "AdobeSym.enc",
               "ComputerModern" = "TeXtext.enc",
               "ComputerModernItalic" = "TeXtext.enc")
    }  else {
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
}

# Attempt to provide a sensible default font for a PostScript
# or PDF device
# In particular, use an "Asian" CID font if in an "Asian" locale
guessFamily <- function()
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
           # Assume that everywhere else, Helvetica is sensible
           "Helvetica")
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

    # need to handle this case before encoding
    if(!missing(family) &&
       (inherits(family, "Type1Font") || inherits(family, "CIDFont"))) {
        enc <- family$encoding
        if(inherits(family, "Type1Font") &&!is.null(enc) && enc != "default"
           && (is.null(old$encoding) || old$encoding  == "default"))
            old$encoding <- enc
        family <- family$metrics
    }
    if(is.null(old$encoding) || old$encoding  == "default")
        old$encoding <- guessEncoding(family)

    ## handle family separately as length can be 1, 4, or 5
    if(!missing(family)) {
        # Case where family is a set of AFMs
        if(length(family) == 4) {
            family <- c(family, "Symbol.afm")
        } else if (length(family) == 5) {
            ## nothing to do
        } else if (length(family) == 1) {
            ## If family has been specified, match with a font in the
            ## font database (see postscriptFonts())
            ## and pass in a device-independent font name.
            ## NOTE that in order to match, we need both family name
            ## and encoding to match.
            matchFont(postscriptFonts(family)[[1]], old$encoding)
        } else
            stop("invalid 'family' argument")
        old$family <- family
    }

    .External("PostScript",
              file, old$paper, old$family, old$encoding, old$bg, old$fg,
              old$width, old$height, old$horizontal, old$pointsize,
              old$onefile, old$pagecentre, old$print.it, old$command,
              title, fonts, PACKAGE = "grDevices")
    # if .ps.prolog is searched for and fails, NULL got returned.
    invisible()
}

xfig <- function (file = ifelse(onefile,"Rplots.fig", "Rplot%03d.fig"),
                  onefile = FALSE, encoding="none", ...)
{
    new <- list(onefile=onefile, ...)# eval
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = FALSE, assign.opt = FALSE)

    .External("XFig",
              file, old$paper, old$family, old$bg, old$fg,
              old$width, old$height, old$horizontal, old$pointsize,
              old$onefile, old$pagecentre, encoding, PACKAGE = "grDevices")
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
    # need to handle this before encoding
    if(!missing(family) &&
       (inherits(family, "Type1Font") || inherits(family, "CIDFont"))) {
        enc <- family$encoding
        if(inherits(family, "Type1Font") &&!is.null(enc) && enc != "default"
           && (is.null(old$encoding) || old$encoding  == "default"))
            old$encoding <- enc
        family <- family$metrics
    }
    if(is.null(old$encoding) || old$encoding  == "default")
        old$encoding <- guessEncoding()
    ## handle family separately as length can be 1, 4, or 5
    if(!missing(family)) {
        # Case where family is a set of AFMs
        if(length(family) == 4) {
            family <- c(family, "Symbol.afm")
        } else if (length(family) == 5) {
            ## nothing to do
        } else if (length(family) == 1) {
            ## If family has been specified, match with a font in the
            ## font database (see postscriptFonts())
            ## and pass in a device-independent font name.
            ## NOTE that in order to match, we need both family name
            ## and encoding to match.
            matchFont(pdfFonts(family)[[1]], old$encoding)
        } else
            stop("invalid 'family' argument")
        old$family <- family
    }
    # Extract version
    versions <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6")
    if (version %in% versions)
        version <- as.integer(strsplit(version, "[.]")[[1]])
    else
        stop("invalid PDF version")
    .External("PDF",
              file, old$paper, old$family, old$encoding, old$bg, old$fg,
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
#
# PostScript fonts may be either Type1 or CID-keyed fonts
# (the latter provides support for CJK fonts)
####################

assign(".PostScript.Fonts", list(), envir = .PSenv)

fontError <- function(errDesc)
    stop(gettextf("invalid %s in font specification", errdesc), domain = NA)

checkFont <- function(font) UseMethod("checkFont")


checkFont.default <- function(font) stop("Invalid font type")

# A Type1 font family has a name, plus a vector of 4 or 5 directories
# for font metric afm files, plus an encoding file

# Check that the font has the correct structure and information
# Already checked that it had a name
checkFont.Type1Font <- function(font) {
    if (is.null(font$family) || !is.character(font$family))
        fontError("font family name")
    if (is.null(font$metrics) || !is.character(font$metrics) ||
        length(font$metrics) < 4)
        fontError("font metric information")
        ## Add default symbol font metric if none provided
    if (length(font$metrics) == 4)
        font$metrics <- c(font$metrics, "Symbol.afm")
    if (is.null(font$encoding) || !is.character(font$encoding))
        fontError("font encoding")
    font
}

# A CID-keyed font family has a name, four afm files,
# a CMap name, a CMap encoding, and (for now at least) a
# PDF chunk
# (I really hope we can dispense with the latter!)
checkFont.CIDFont <- function(font) {
    if (!inherits(font, "CIDFont"))
        stop("Not a CID font")
    if (is.null(font$family) || !is.character(font$family))
        fontError("font family name")
    if (is.null(font$metrics) || !is.character(font$metrics) ||
        length(font$metrics) < 4)
        fontError("font metric information")
        ## Add default symbol font metric if none provided
    if (length(font$metrics) == 4)
        font$metrics <- c(font$metrics, "Symbol.afm")
    if (is.null(font$cmap) || !is.character(font$cmap))
        fontError("CMap name")
    if (is.null(font$cmapEncoding) || !is.character(font$cmapEncoding))
        fontError("font cmapEncoding")
    if (is.null(font$pdfresource) || !is.character(font$pdfresource))
        fontError("PDF resource")
    font
}

isPDF <- function(fontDBname) {
  switch(fontDBname,
         .PostScript.Fonts=FALSE,
         .PDF.Fonts=TRUE,
         stop("Invalid font database name"))
}

checkFontInUse <- function(names, fontDBname) {
    for (i in names)
        if (.Call("Type1FontInUse", i, isPDF(fontDBname),
                  PACKAGE = "grDevices") ||
            .Call("CIDFontInUse", i, isPDF(fontDBname),
                  PACKAGE = "grDevices"))
            stop(gettextf("font %s already in use", i), domain = NA)
}

setFonts <- function(fonts, fontNames, fontDBname) {
    fonts <- lapply(fonts, checkFont)
    fontDB <- get(fontDBname, envir=.PSenv)
    existingFonts <- fontNames %in% names(fontDB)
    if (sum(existingFonts) > 0) {
        checkFontInUse(fontNames[existingFonts], fontDBname)
        fontDB[fontNames[existingFonts]] <- fonts[existingFonts]
    }
    if (sum(existingFonts) < length(fontNames))
        fontDB <- c(fontDB, fonts[!existingFonts])
    assign(fontDBname, fontDB, envir=.PSenv)
}

printFont <- function(font) UseMethod("printFont")

printFont.Type1Font <- function(font)
    paste(font$family, "\n    (", paste(font$metrics, collapse=" "),
          "\n    ", font$encoding, "\n", sep="")

printFont.CIDFont <- function(font)
    paste(font$family, "\n    (", paste(font$metrics, collapse=" "),
          ")\n    ", font$CMap, "\n    ", font$encoding, "\n", sep="")

printFonts <- function(fonts)
    cat(paste(names(fonts), ": ", unlist(lapply(fonts, printFont)),
              sep="", collapse=""))

# If no arguments specified, return entire font database
# If no named arguments specified, all args should be font names
# on which to get info from the database
# Else, must specify new fonts to enter into database (all
# of which must be valid PostScript font descriptions and
# all of which must be named args)
postscriptFonts <- function(...)
{
    ndots <- length(fonts <- list(...))
    if (ndots == 0)
        get(".PostScript.Fonts", envir=.PSenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0) {
            if (!all(sapply(fonts, is.character)))
                stop(gettextf("invalid arguments in '%s' (must be font names)",
                              "postscriptFonts"), domain = NA)
            else
                get(".PostScript.Fonts", envir=.PSenv)[unlist(fonts)]
        } else {
            if (ndots != nnames)
                stop(gettextf("invalid arguments in '%s' (need named args)",
                              "postscriptFonts"), domain = NA)
            setFonts(fonts, fontNames, ".PostScript.Fonts")
        }
    }
}

# Create a valid postscript font description
Type1Font <- function(family, metrics, encoding="default")
{
    font <- list(family=family, metrics=metrics, encoding=encoding)
    class(font) <- "Type1Font"
    checkFont(font)
}

CIDFont <- function(family, metrics, cmap, cmapEncoding, pdfresource="")
{
    font <- list(family=family, metrics=metrics, cmap=cmap,
                 cmapEncoding=cmapEncoding, pdfresource=pdfresource)
    class(font) <- "CIDFont"
    checkFont(font)
}

# Deprecated in favour of Type1Font()
postscriptFont <- function(family, metrics, encoding="default")
{
    .Deprecated("Type1Font")
    Type1Font(family, metrics, encoding)
}


####################
# PDF font database
#
# PDF fonts may be either Type1 or CID-keyed fonts
# (the latter provides support for CJK fonts)
#
# PDF font database separate from PostScript one because
# some standard CID fonts are different
####################

assign(".PDF.Fonts", list(), envir = .PSenv)

pdfFonts <- function(...)
{
    ndots <- length(fonts <- list(...))
    if (ndots == 0)
        get(".PDF.Fonts", envir=.PSenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0) {
            if (!all(sapply(fonts, is.character)))
                stop(gettextf("invalid arguments in '%s' (must be font names)",
                              "pdfFonts"), domain = NA)
            else
                get(".PDF.Fonts", envir=.PSenv)[unlist(fonts)]
        } else {
            if (ndots != nnames)
                stop(gettextf("invalid arguments in '%s' (need named args)",
                              "pdfFonts"), domain = NA)
            setFonts(fonts, fontNames, ".PDF.Fonts")
        }
    }
}

# Match an encoding
# NOTE that if encoding in font database is "default", that is a match
matchEncoding <- function(font, encoding) UseMethod("matchEncoding")

matchEncoding.Type1Font <- function(font, encoding) {
    ## the trailing .enc is optional
    font$encoding %in% c("default", encoding, paste(encoding, ".enc", sep=""))
}

# Users should not be specifying a CID font AND an encoding
# when starting a new device
matchEncoding.CIDFont <- function(font, encoding) TRUE

# Match a font name (and an encoding)
matchFont <- function(font, encoding) {
    if (is.null(font))
        stop("unknown font")
    if (!matchEncoding(font, encoding))
        stop(gettextf("font encoding mismatch '%s'/'%s'",
                      font$encoding, encoding), domain=NA)
}

# Function to initialise default PostScript and PDF fonts
# Called in .onLoad
# NOTE that this is in .onLoad
#   a) because that's a sensible place to do initialisation of package globals
#   b) because it does not work to do it BEFORE then.  In particular,
#      if the body of this function is evaluated when the R code of the
#      package is sourced, then the method dispatch on checkFont() does
#      not work because when the R code is sourced, the S3 methods in
#      this package have not yet been registered.
#      Also, we want the run-time locale not the install-time locale.

initPSandPDFfonts <- function() {

assign(".PostScript.Options",
    list(paper	= "default",
	 horizontal = TRUE,
	 width	= 0,
	 height = 0,
	 family = guessFamily(),
	 encoding = "default",
	 pointsize  = 12,
	 bg	= "transparent",
	 fg	= "black",
	 onefile    = TRUE,
	 print.it   = FALSE,
	 append	    = FALSE,
	 pagecentre = TRUE,
	 command    = "default"), envir = .PSenv)

postscriptFonts(# Default Serif font is Times
                serif=Type1Font("Times",
                  c("Times-Roman.afm", "Times-Bold.afm",
                    "Times-Italic.afm", "Times-BoldItalic.afm",
                    "Symbol.afm")),
                # Default Sans Serif font is Helvetica
                sans=Type1Font("Helvetica",
                  c("Helvetica.afm", "Helvetica-Bold.afm",
                    "Helvetica-Oblique.afm", "Helvetica-BoldOblique.afm",
                    "Symbol.afm")),
                # Default Monospace font is Courier
                mono=Type1Font("Courier",
                  c("Courier.afm", "Courier-Bold.afm",
                    "Courier-Oblique.afm", "Courier-BoldOblique.afm",
                    "Symbol.afm")),
                # Default Symbol font is Symbol
                symbol=Type1Font("Symbol",
                  c("Symbol.afm", "Symbol.afm", "Symbol.afm", "Symbol.afm",
                    "Symbol.afm"), encoding="AdobeSym.enc"),
                # Remainder are standard Adobe fonts that
                # should be present on PostScript devices
                AvantGarde=Type1Font("AvantGarde",
                  c("agw_____.afm", "agd_____.afm",
                    "agwo____.afm", "agdo____.afm",
                    "Symbol.afm")),
                Bookman=Type1Font("Bookman",
                  c("bkl_____.afm", "bkd_____.afm",
                    "bkli____.afm", "bkdi____.afm",
                    "Symbol.afm")),
                Courier=Type1Font("Courier",
                  c("Courier.afm", "Courier-Bold.afm",
                    "Courier-Oblique.afm", "Courier-BoldOblique.afm",
                    "Symbol.afm")),
                Helvetica=Type1Font("Helvetica",
                  c("Helvetica.afm", "Helvetica-Bold.afm",
                    "Helvetica-Oblique.afm", "Helvetica-BoldOblique.afm",
                    "Symbol.afm")),
                "Helvetica-Narrow"=Type1Font("Helvetica-Narrow",
                  c("hvn_____.afm", "hvnb____.afm",
                    "hvno____.afm", "hvnbo___.afm",
                    "Symbol.afm")),
                NewCenturySchoolbook=Type1Font("NewCenturySchoolbook",
                  c("ncr_____.afm", "ncb_____.afm",
                    "nci_____.afm", "ncbi____.afm",
                    "Symbol.afm")),
                Palatino=Type1Font("Palatino",
                  c("por_____.afm", "pob_____.afm",
                    "poi_____.afm", "pobi____.afm",
                    "Symbol.afm")),
                Times=Type1Font("Times",
                  c("Times-Roman.afm", "Times-Bold.afm",
                    "Times-Italic.afm", "Times-BoldItalic.afm",
                    "Symbol.afm")),
                # URW equivalents
                URWGothic=Type1Font("URWGothic",
                  c("a010013l.afm", "a010015l.afm",
                    "a010033l.afm", "a010035l.afm",
                    "s050000l.afm")),
                URWBookman=Type1Font("URWBookman",
                  c("b018012l.afm", "b018015l.afm",
                    "b018032l.afm", "b018035l.afm",
                    "s050000l.afm")),
                NimbusMon=Type1Font("NimbusMon",
                  c("n022003l.afm", "n022004l.afm",
                    "n022023l.afm", "n022024l.afm",
                    "s050000l.afm")),
                NimbusSan=Type1Font("NimbusSan",
                  c("n019003l.afm", "n019004l.afm",
                    "n019023l.afm", "n019024l.afm",
                    "s050000l.afm")),
                URWHelvetica=Type1Font("URWHelvetica",
                  c("n019003l.afm", "n019004l.afm",
                    "n019023l.afm", "n019024l.afm",
                    "s050000l.afm")),
                NimbusSanCond=Type1Font("NimbusSanCond",
                  c("n019043l.afm", "n019044l.afm",
                    "n019063l.afm", "n019064l.afm",
                    "s050000l.afm")),
                CenturySch=Type1Font("CenturySch",
                  c("c059013l.afm", "c059016l.afm",
                    "c059033l.afm", "c059036l.afm",
                    "s050000l.afm")),
                URWPalladio=Type1Font("URWPalladio",
                  c("p052003l.afm", "p052004l.afm",
                    "p052023l.afm", "p052024l.afm",
                    "s050000l.afm")),
                NimbusRom=Type1Font("NimbusRom",
                  c("n021003l.afm", "n021004l.afm",
                    "n021023l.afm", "n021024l.afm",
                    "s050000l.afm")),
                URWTimes=Type1Font("URWTimes",
                  c("n021003l.afm", "n021004l.afm",
                    "n021023l.afm", "n021024l.afm",
                    "s050000l.afm"))
                )

## All of the above Type1 fonts are the same for PostScript and PDF
do.call("pdfFonts", postscriptFonts())

## add ComputerModern to postscript only
postscriptFonts(# Computer Modern as recoded by Brian D'Urso
                ComputerModern=Type1Font("ComputerModern",
                  c("CM_regular_10.afm", "CM_boldx_10.afm",
                    "CM_italic_10.afm", "CM_boldx_italic_10.afm",
                    "CM_symbol_10.afm"), encoding = "TeXtext.enc"),
                 ComputerModernItalic=Type1Font("ComputerModernItalic",
                  c("CM_regular_10.afm", "CM_boldx_10.afm", "cmti10.afm",
                    "cmbxti10.afm", "CM_symbol_10.afm"),
                 encoding = "TeXtext.enc")
                )


# CJK fonts
postscriptFonts(Japan1=CIDFont("HeiseiKakuGo-W5",
                  c("Adobe-Japan1-UniJIS-UCS2-H.afm",
                    "Adobe-Japan1-UniJIS-UCS2-H.afm",
                    "Adobe-Japan1-UniJIS-UCS2-H.afm",
                    "Adobe-Japan1-UniJIS-UCS2-H.afm"),
                  "EUC-H",
                  "EUC-JP"),
                Japan1HeiMin=CIDFont("HeiseiMin-W3",
                  c("Adobe-Japan1-UniJIS-UCS2-H.afm",
                    "Adobe-Japan1-UniJIS-UCS2-H.afm",
                    "Adobe-Japan1-UniJIS-UCS2-H.afm",
                    "Adobe-Japan1-UniJIS-UCS2-H.afm"),
                  "EUC-H",
                  "EUC-JP"),
                Japan1GothicBBB=CIDFont("GothicBBB-Medium",
                  c("GothicBBB-Medium-UCS2-H.afm",
                    "GothicBBB-Medium-UCS2-H.afm",
                    "GothicBBB-Medium-UCS2-H.afm",
                    "GothicBBB-Medium-UCS2-H.afm"),
                  "EUC-H",
                  "EUC-JP"),
                Japan1Ryumin=CIDFont("Ryumin-Light",
                c("Ryumin-Light-UCS2-H.afm",
                  "Ryumin-Light-UCS2-H.afm",
                  "Ryumin-Light-UCS2-H.afm",
                  "Ryumin-Light-UCS2-H.afm"),
                  "EUC-H",
                  "EUC-JP"),
                Korea1=CIDFont("Baekmuk-Batang",
                  c("Adobe-Korea1-UniKS-UCS2-H.afm",
                    "Adobe-Korea1-UniKS-UCS2-H.afm",
                    "Adobe-Korea1-UniKS-UCS2-H.afm",
                    "Adobe-Korea1-UniKS-UCS2-H.afm"),
                  "KSCms-UHC-H",
                  "CP949"),
                Korea1deb=CIDFont("Batang-Regular",
                  c("Adobe-Korea1-UniKS-UCS2-H.afm",
                    "Adobe-Korea1-UniKS-UCS2-H.afm",
                    "Adobe-Korea1-UniKS-UCS2-H.afm",
                    "Adobe-Korea1-UniKS-UCS2-H.afm"),
                  "KSCms-UHC-H",
                  "CP949"),
                CNS1=CIDFont("MOESung-Regular",
                  c("Adobe-CNS1-UniCNS-UCS2-H.afm",
                    "Adobe-CNS1-UniCNS-UCS2-H.afm",
                    "Adobe-CNS1-UniCNS-UCS2-H.afm",
                    "Adobe-CNS1-UniCNS-UCS2-H.afm"),
                  "B5pc-H",
                  "CP950"),
                GB1=CIDFont("BousungEG-Light-GB",
                  c("Adobe-GB1-UniGB-UCS2-H.afm",
                    "Adobe-GB1-UniGB-UCS2-H.afm",
                    "Adobe-GB1-UniGB-UCS2-H.afm",
                    "Adobe-GB1-UniGB-UCS2-H.afm"),
                  "GBK-EUC-H",
                  "GBK"))

pdfFonts(Japan1=CIDFont("KozMinPro-Regular-Acro",
           c("Adobe-Japan1-UniJIS-UCS2-H.afm",
             "Adobe-Japan1-UniJIS-UCS2-H.afm",
             "Adobe-Japan1-UniJIS-UCS2-H.afm",
             "Adobe-Japan1-UniJIS-UCS2-H.afm"),
           "EUC-H",
           "EUC-JP",
           paste("/FontDescriptor",
                 "<<",
                 "  /Type /FontDescriptor",
                 "  /CapHeight 740 /Ascent 1075 /Descent -272 /StemV 72",
                 "  /FontBBox [-195 -272 1110 1075]",
                 "  /ItalicAngle 0 /Flags 6 /XHeight 502",
                 "  /Style << /Panose <000001000500000000000000> >>",
                 ">>",
                 "/CIDSystemInfo << /Registry(Adobe) /Ordering(Japan1) /Supplement  2 >>",
                 "/DW 1000",
                 "/W [",
                 "   1 632 500 ",
                 "   8718 [500 500] ",
                 "]\n",
                 sep="\n      ")),
         Japan1HeiMin=CIDFont("HeiseiMin-W3-Acro",
           c("Adobe-Japan1-UniJIS-UCS2-H.afm",
             "Adobe-Japan1-UniJIS-UCS2-H.afm",
             "Adobe-Japan1-UniJIS-UCS2-H.afm",
             "Adobe-Japan1-UniJIS-UCS2-H.afm"),
           "EUC-H",
           "EUC-JP",
           paste("/FontDescriptor",
                 "<<",
                 "  /Type /FontDescriptor",
                 "  /CapHeight 709 /Ascent 723 /Descent -241 /StemV 69",
                 "  /FontBBox [-123 -257 1001 910]",
                 "  /ItalicAngle 0 /Flags 6 /XHeight 450",
                 "  /Style << /Panose <000002020500000000000000> >>",
                 ">>",
                 "/CIDSystemInfo << /Registry(Adobe) /Ordering(Japan1) /Supplement  2 >>",
                 "/DW 1000",
                 "/W [",
                 "   1 632 500 ",
                 "   8718 [500 500] ",
                 "]\n",
                 sep="\n      ")),
         Japan1GothicBBB=CIDFont("GothicBBB-Medium",
           c("GothicBBB-Medium-UCS2-H.afm",
             "GothicBBB-Medium-UCS2-H.afm",
             "GothicBBB-Medium-UCS2-H.afm",
             "GothicBBB-Medium-UCS2-H.afm"),
           "EUC-H",
           "EUC-JP",
           paste("/FontDescriptor",
                 "<<",
                 "  /Type /FontDescriptor",
                 "  /CapHeight 737 /Ascent 752 /Descent -271 /StemV 99",
                 "  /FontBBox [-22 -252 1000 892]",
                 "  /ItalicAngle 0 /Flags 4",
                 "  /Style << /Panose <0801020b0500000000000000> >>",
                 ">>",
                 "/CIDSystemInfo << /Registry(Adobe) /Ordering(Japan1) /Supplement  2 >>",
                 "/DW 1000",
                 "/W [",
                 "   1 632 500",
                 "   8718 [500 500]",
                 "]\n",
                 sep="\n      ")),
         Japan1Ryumin=CIDFont("Ryumin-Light",
           c("Ryumin-Light-UCS2-H.afm",
             "Ryumin-Light-UCS2-H.afm",
             "Ryumin-Light-UCS2-H.afm",
             "Ryumin-Light-UCS2-H.afm"),
           "EUC-H",
           "EUC-JP",
           paste("/FontDescriptor",
                 "<<",
                 "  /Type /FontDescriptor",
                 "  /CapHeight 709 /Ascent 723 /Descent -241 /StemV 69",
                 "  /FontBBox [-54 -305 1000 903]",
                 "  /ItalicAngle 0 /Flags 6",
                 "  /Style << /Panose <010502020300000000000000> >>",
                 ">>",
                 "/CIDSystemInfo << /Registry(Adobe) /Ordering(Japan1) /Supplement  2 >>",
                 "/DW 1000",
                 "/W [",
                 "   1 632 500",
                 "   8718 [500 500]",
                 "]\n",
                 sep="\n      ")),
         Korea1=CIDFont("HYSMyeongJoStd-Medium-Acro",
           c("Adobe-Korea1-UniKS-UCS2-H.afm",
             "Adobe-Korea1-UniKS-UCS2-H.afm",
             "Adobe-Korea1-UniKS-UCS2-H.afm",
             "Adobe-Korea1-UniKS-UCS2-H.afm"),
           "KSCms-UHC-H",
           "CP949",
           paste("/FontDescriptor",
                 "<<",
                 "  /Type /FontDescriptor",
                 "  /CapHeight 720 /Ascent 880 /Descent -148 /StemV 59",
                 "  /FontBBox [-28 -148 1001 880]",
                 "  /ItalicAngle 0 /Flags 6 /XHeight 468",
                 "  /Style << /Panose <000001000600000000000000> >>",
                 ">>",
                 "/CIDSystemInfo << /Registry(Adobe) /Ordering(Korea1) /Supplement 1 >>",
                 "/DW 1000",
                 "/W [",
                 "   1 94 500",
                 "   97 [500] ",
                 "   8094 8190 500",
                 "]\n",
                 sep="\n      ")),
         Korea1deb=CIDFont("HYGothic-Medium-Acro",
           c("Adobe-Korea1-UniKS-UCS2-H.afm",
             "Adobe-Korea1-UniKS-UCS2-H.afm",
             "Adobe-Korea1-UniKS-UCS2-H.afm",
             "Adobe-Korea1-UniKS-UCS2-H.afm"),
           "KSCms-UHC-H",
           "CP949",
           paste("/FontDescriptor",
                 "<<",
                 "  /Type /FontDescriptor",
                 "  /CapHeight 737 /Ascent 752 /Descent -271 /StemV 58",
                 "  /FontBBox [-6 -145 1003 880]",
                 "  /ItalicAngle 0 /Flags 4 /XHeight 553",
                 "  /Style << /Panose <000001000600000000000000> >>",
                 ">>",
                 "/CIDSystemInfo << /Registry(Adobe) /Ordering(Korea1) /Supplement 1 >>",
                 "/DW 1000",
                 "/W [",
                 "   1 94 500",
                 "   97 [500] ",
                 "   8094 8190 500",
                 "]\n",
                 sep="\n      ")),
         CNS1=CIDFont("MSungStd-Light-Acro",
           c("Adobe-CNS1-UniCNS-UCS2-H.afm",
             "Adobe-CNS1-UniCNS-UCS2-H.afm",
             "Adobe-CNS1-UniCNS-UCS2-H.afm",
             "Adobe-CNS1-UniCNS-UCS2-H.afm"),
           "B5pc-H",
           "CP950",
           paste("/FontDescriptor",
                 "<<",
                 "  /Type /FontDescriptor",
                 "  /CapHeight 662 /Ascent 1071 /Descent -249 /StemV 66",
                 "  /FontBBox [-160 -249 1015 1071]",
                 "  /ItalicAngle 0 /Flags 6 /XHeight 400",
                 "  /Style << /Panose <000001000600000000000000> >>",
                 ">>",
                 "/CIDSystemInfo << /Registry(Adobe) /Ordering(CNS1) /Supplement  0 >>",
                 "/DW 1000",
                 "/W [",
                 "     1 33 500",
                 "     34 [749 673 679 679 685 671 738 736 333 494 739 696 902 720 750 674 746 672 627 769 707 777 887 709 616]",
                 "     60 65 500",
                 "     66 [500 511 502 549 494 356 516 550 321 321 510 317 738 533 535 545 533 376 443 261 529 742 534 576 439]",
                 "     92 95 500",
                 "     13648 13742 500",
                 "     17603 [500]",
                 "]\n",
                 sep="\n      ")),
         GB1=CIDFont("STSong-Light-Acro",
           c("Adobe-GB1-UniGB-UCS2-H.afm",
             "Adobe-GB1-UniGB-UCS2-H.afm",
             "Adobe-GB1-UniGB-UCS2-H.afm",
             "Adobe-GB1-UniGB-UCS2-H.afm"),
           "GBK-EUC-H",
           "GBK",
           paste("/FontDescriptor",
                 "<<",
                 "  /Type /FontDescriptor",
                 "  /CapHeight 626 /Ascent 905 /Descent -254 /StemV 48",
                 "  /FontBBox [-134 -254 1001 905]",
                 "  /ItalicAngle 0 /Flags 6 /XHeight 416",
                 "  /Style << /Panose <000000000400000000000000> >>",
                 ">>",
                 "/CIDSystemInfo << /Registry(Adobe) /Ordering(GB1) /Supplement  2 >>",
                 "/DW 1000",
                 "/W [",
                 "     1 95 500",
                 "     814 939 500",
                 "     7712 7716 500",
                 "     22127 22357 500",
                 "]\n",
                 sep="\n      ")))
}
