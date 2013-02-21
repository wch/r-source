#  File src/library/grDevices/R/postscript.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## An environment not exported from namespace:graphics used to pass
## .PostScript.Options and .PDF.options to the windows() device for
## use in its menus, and also to hide the variables.
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
    old <- get(name.opt, envir=envir, inherits=FALSE)
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
                    ## skip 'fonts';
                    ii <- ii & (names(prev) != "fonts")
                    if(!any(ii)) next
		    doubt <- doubt | ii
		    do.keep <- ii & !override.check
		    warning(paste(sQuote(paste0(fn, "(", names(prev[ii]), ")" )),
                                  collapse = " and "), " ",
                            ngettext(as.integer(sum(ii)),
                                     "differs between new and previous",
                                     "differ between new and previous"),
                            if(any(do.keep)) {
                                paste("\n\t ==> ",
                                      gettextf("NOT changing %s",
                                              paste(sQuote(names(prev[do.keep])),
                                                    collapse=" & ")),
                                      sep = "")} else "",
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

ps.options <- function(..., reset = FALSE, override.check = FALSE)
{
    ## do initialization if needed
    initPSandPDFfonts()
    old <- get(".PostScript.Options", envir = .PSenv)
    if(reset) {
        assign(".PostScript.Options",
               get(".PostScript.Options.default", envir = .PSenv),
               envir = .PSenv)
    }
    l... <- length(new <- list(...))
    if(m <- match("append", names(new), 0L)) {
        warning("argument 'append' is for back-compatibility and will be ignored",
                immediate. = TRUE)
        new <- new[-m]
    }
    check.options(new, name.opt = ".PostScript.Options", envir = .PSenv,
                  assign.opt = l... > 0, override.check = override.check)
    if(reset || l... > 0) invisible(old) else old
}

setEPS <- function(...)
{
    dots <- list(...)
    args <- list(width = 7, height = 7)
    args[names(dots)] <- dots
    force <- list(onefile = FALSE, horizontal = FALSE, paper = "special")
    args[names(force)] <- force
    do.call("ps.options", args)
}

setPS <- function(...)
{
    dots <- list(...)
    args <- list(width = 0, height = 0)
    args[names(dots)] <- dots
    force <- list(onefile = TRUE, horizontal = TRUE, paper = "default")
    args[names(force)] <- force
    do.call("ps.options", args)
}

pdf.options <- function(..., reset=FALSE)
{
    ## do initialization if needed
    initPSandPDFfonts()
    old <- get(".PDF.Options", envir = .PSenv)
    if(reset) {
        assign(".PDF.Options",
               get(".PDF.Options.default", envir = .PSenv),
               envir = .PSenv)
    }
    l... <- length(new <- list(...))
    check.options(new, name.opt = ".PDF.Options", envir = .PSenv,
                  assign.opt = l... > 0)
    if(reset || l... > 0) invisible(old) else old
}

guessEncoding <- function(family)
{
    # Three special families have special encodings, regardless of locale
    if (!missing(family) &&
        family %in% c("ComputerModern", "ComputerModernItalic")) {
        switch(family,
               "ComputerModern" = "TeXtext.enc",
               "ComputerModernItalic" = "TeXtext.enc")
    }  else {
        switch(.Platform$OS.type,
               "windows" = {
                   switch(utils::localeToCharset()[1L],
                          "ISO8859-2" = "CP1250.enc",
                          "ISO8859-7" = "CP1253.enc", # Greek
                          "ISO8859-13" = "CP1257.enc",
                          "CP1251" = "CP1251.enc", # Cyrillic
                          "WinAnsi.enc")
               },
               { lc <- utils::localeToCharset()
                 if(length(lc) == 1L)
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
                 else if(lc[1L] == "UTF-8")
                     switch(lc[2L],
                            "ISO8859-1" = "ISOLatin1.enc", # what about Euro?
                            "ISO8859-2" = "ISOLatin2.enc",
                            "ISO8859-5" = "Cyrillic.enc",
                            "ISO8859-7" = "Greek.enc",
                            "ISO8859-13" = "ISOLatin7.enc",
                            "ISOLatin1.enc")
                 else "ISOLatin1.enc"})
    }
}

##--> source in devPS.c :

postscript <- function(file = ifelse(onefile, "Rplots.ps", "Rplot%03d.ps"),
                       onefile, family, title , fonts, encoding, bg, fg,
                       width, height, horizontal, pointsize,
                       paper, pagecentre, print.it, command, colormodel,
                       useKerning, fillOddEven)
{
    ## do initialization if needed
    initPSandPDFfonts()

    new <- list()
    if(!missing(onefile)) new$onefile <- onefile
    ## 'family' is handled separately
    if(!missing(title)) new$title <- title
    if(!missing(fonts)) new$fonts <- fonts
    if(!missing(encoding)) new$encoding <- encoding
    if(!missing(bg)) new$bg <- bg
    if(!missing(fg)) new$fg <- fg
    if(!missing(width)) new$width <- width
    if(!missing(height)) new$height <- height
    if(!missing(horizontal)) new$horizontal <- horizontal
    if(!missing(pointsize)) new$pointsize <- pointsize
    if(!missing(paper)) new$paper <- paper
    if(!missing(pagecentre)) new$pagecentre <- pagecentre
    if(!missing(print.it)) new$print.it <- print.it
    if(!missing(command)) new$command <- command
    if(!missing(colormodel)) new$colormodel <- colormodel
    if(!missing(useKerning)) new$useKerning <- useKerning
    if(!missing(fillOddEven)) new$fillOddEven <- fillOddEven

    old <- check.options(new, name.opt = ".PostScript.Options", envir = .PSenv)

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
        if(length(family) == 4L) {
            family <- c(family, "Symbol.afm")
        } else if (length(family) == 5L) {
            ## nothing to do
        } else if (length(family) == 1L) {
            ## If family has been specified, match with a font in the
            ## font database (see postscriptFonts())
            ## and pass in a device-independent font name.
            ## NOTE that in order to match, we need both family name
            ## and encoding to match.
            pf <- postscriptFonts(family)[[1L]]
            if(is.null(pf))
              stop(gettextf("unknown family '%s'", family), domain = NA)
            matchFont(pf, old$encoding)
        } else
            stop("invalid 'family' argument")
        old$family <- family
    }

    onefile <- old$onefile # for 'file'
    if(!checkIntFormat(file)) stop("invalid 'file'")
    .External(C_PostScript,
              file, old$paper, old$family, old$encoding, old$bg, old$fg,
              old$width, old$height, old$horizontal, old$pointsize,
              onefile, old$pagecentre, old$print.it, old$command,
              old$title, old$fonts, old$colormodel, old$useKerning,
              old$fillOddEven)
    # if .ps.prolog is searched for and fails, NULL got returned.
    invisible()
}

xfig <- function (file = ifelse(onefile,"Rplots.fig", "Rplot%03d.fig"),
                  onefile = FALSE, encoding = "none",
                  paper = "default", horizontal = TRUE,
                  width = 0, height = 0, family = "Helvetica",
                  pointsize = 12, bg = "transparent", fg = "black",
                  pagecentre = TRUE,
                  defaultfont = FALSE, textspecial = FALSE)
{
    ## do initialization if needed
    initPSandPDFfonts()

    if(!checkIntFormat(file)) stop("invalid 'file'")
    .External(C_XFig, file, paper, family, bg, fg,
              width, height, horizontal, pointsize,
              onefile, pagecentre, defaultfont, textspecial, encoding)
    invisible()
}

pdf <- function(file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"),
                width, height, onefile, family, title, fonts, version,
                paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
                useDingbats, useKerning, fillOddEven, compress)
{
    ## do initialization if needed
    initPSandPDFfonts()

    new <- list()
    if(!missing(width)) new$width <- width
    if(!missing(height)) new$height <- height
    if(!missing(onefile)) new$onefile <- onefile
    ## 'family' is handled separately
    if(!missing(title)) new$title <- title
    if(!missing(fonts)) new$fonts <- fonts
    if(!missing(version)) new$version <- version
    if(!missing(paper)) new$paper <- paper
    if(!missing(encoding)) new$encoding <- encoding
    if(!missing(bg)) new$bg <- bg
    if(!missing(fg)) new$fg <- fg
    if(!missing(pointsize)) new$pointsize <- pointsize
    if(!missing(pagecentre)) new$pagecentre <- pagecentre
    if(!missing(colormodel)) new$colormodel <- colormodel
    if(!missing(useDingbats)) new$useDingbats <- useDingbats
    if(!missing(useKerning)) new$useKerning <- useKerning
    if(!missing(fillOddEven)) new$fillOddEven <- fillOddEven
    if(!missing(compress)) new$compress <- compress

    old <- check.options(new, name.opt = ".PDF.Options", envir = .PSenv)

    ## need to handle this before encoding
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
        if(length(family) == 4L) {
            family <- c(family, "Symbol.afm")
        } else if (length(family) == 5L) {
            ## nothing to do
        } else if (length(family) == 1L) {
            ## If family has been specified, match with a font in the
            ## font database (see postscriptFonts())
            ## and pass in a device-independent font name.
            ## NOTE that in order to match, we need both family name
            ## and encoding to match.
            pf <- pdfFonts(family)[[1L]]
            if(is.null(pf))
              stop(gettextf("unknown family '%s'", family), domain = NA)
            matchFont(pf, old$encoding)
        } else
            stop("invalid 'family' argument")
        old$family <- family
    }
    # Extract version
    version <- old$version
    versions <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", "2.0")
    if (version %in% versions)
        version <- as.integer(strsplit(version, "[.]")[[1L]])
    else
        stop("invalid PDF version")

    onefile <- old$onefile # needed to set 'file'
    if(!checkIntFormat(file)) stop("invalid 'file'")
    .External(C_PDF,
              file, old$paper, old$family, old$encoding, old$bg, old$fg,
              old$width, old$height, old$pointsize, onefile, old$pagecentre,
              old$title, old$fonts, version[1L], version[2L],
              old$colormodel, old$useDingbats, old$useKerning,
              old$fillOddEven, old$compress)
    invisible()
}

.ps.prolog <- c(
"/gs  { gsave } bind def",
"/gr  { grestore } bind def",
"/ep  { showpage gr gr } bind def",
"/m   { moveto } bind def",
"/l  { rlineto } bind def",
"/np  { newpath } bind def",
"/cp  { closepath } bind def",
"/f   { fill } bind def",
"/o   { stroke } bind def",
"/c   { newpath 0 360 arc } bind def",
"/r   { 4 2 roll moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch rlineto closepath } bind def",
"/p1  { stroke } bind def",
"/p2  { gsave bg fill grestore newpath } bind def",
"/p3  { gsave bg fill grestore stroke } bind def",
"/p6  { gsave bg eofill grestore newpath } bind def",
"/p7  { gsave bg eofill grestore stroke } bind def",
"/t   { 5 -2 roll moveto gsave rotate",
"       1 index stringwidth pop",
"       mul neg 0 rmoveto show grestore } bind def",
"/ta  { 4 -2 roll moveto gsave rotate show } bind def",
"/tb  { 2 -1 roll 0 rmoveto show } bind def",
"/cl  { grestore gsave newpath 3 index 3 index moveto 1 index",
"       4 -1 roll lineto  exch 1 index lineto lineto",
"       closepath clip newpath } bind def",
"/rgb { setrgbcolor } bind def",
"/s   { scalefont setfont } bind def")

.ps.prolog.srgb <- c(## From PLRM 3rd Ed pg 225
"/sRGB { [ /CIEBasedABC",
"          << /DecodeLMN",
"               [ { dup 0.03928 le",
"                        {12.92321 div}",
"                        {0.055 add 1.055 div 2.4 exp }",
"                     ifelse",
"                 } bind dup dup",
"               ]",
"             /MatrixLMN [0.412457 0.212673 0.019334",
"                         0.357576 0.715152 0.119192",
"                         0.180437 0.072175 0.950301]",
"             /WhitePoint [0.9505 1.0 1.0890]",
"           >>",
"         ] setcolorspace } bind def"
)

####################
# PostScript font database
#
# PostScript fonts may be either Type1 or CID-keyed fonts
# (the latter provides support for CJK fonts)
####################

assign(".PostScript.Fonts", list(), envir = .PSenv)

checkFont <- function(font) UseMethod("checkFont")

checkFont.default <- function(font) stop("Invalid font type")

# A Type1 font family has a name, plus a vector of 4 or 5 directories
# for font metric afm files, plus an encoding file

# Check that the font has the correct structure and information
# Already checked that it had a name
checkFont.Type1Font <- function(font) {
    if (is.null(font$family) || !is.character(font$family))
        stop("invalid family name in font specification")
    if (is.null(font$metrics) || !is.character(font$metrics) ||
        length(font$metrics) < 4L)
        stop("invalid metric information in font specification")
        ## Add default symbol font metric if none provided
    if (length(font$metrics) == 4L)
        font$metrics <- c(font$metrics, "Symbol.afm")
    if (is.null(font$encoding) || !is.character(font$encoding))
        stop("invalid encoding in font specification")
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
        stop("invalid family name in font specification")
    if (is.null(font$metrics) || !is.character(font$metrics) ||
        length(font$metrics) < 4L)
        stop("invalid metric information in font specification")
        ## Add default symbol font metric if none provided
    if (length(font$metrics) == 4L)
        font$metrics <- c(font$metrics, "Symbol.afm")
    if (is.null(font$cmap) || !is.character(font$cmap))
        stop("invalid CMap name in font specification")
    if (is.null(font$cmapEncoding) || !is.character(font$cmapEncoding))
        stop("invalid 'cmapEncoding' in font specification")
    if (is.null(font$pdfresource) || !is.character(font$pdfresource))
        stop("invalid PDF resource in font specification")
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
        if (.Call(C_Type1FontInUse, i, isPDF(fontDBname))
            || .Call(C_CIDFontInUse, i, isPDF(fontDBname)))
            stop(gettextf("font %s already in use", i), domain = NA)
    invisible()
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
    paste0(font$family, "\n    (", paste(font$metrics, collapse = " "),
           "\n    ", font$encoding, "\n")

printFont.CIDFont <- function(font)
    paste0(font$family, "\n    (", paste(font$metrics, collapse = " "),
           ")\n    ", font$CMap, "\n    ", font$encoding, "\n")

printFonts <- function(fonts)
    cat(paste(names(fonts), ": ", unlist(lapply(fonts, printFont)),
              sep = "", collapse = ""))

# If no arguments specified, return entire font database
# If no named arguments specified, all args should be font names
# on which to get info from the database
# Else, must specify new fonts to enter into database (all
# of which must be valid PostScript font descriptions and
# all of which must be named args)
postscriptFonts <- function(...)
{
    ## do initialization if needed: not recursive
    initPSandPDFfonts()
    ndots <- length(fonts <- list(...))
    if (ndots == 0L)
        get(".PostScript.Fonts", envir=.PSenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0L) {
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

CIDFont <- function(family, cmap, cmapEncoding, pdfresource="")
{
    font <- list(family=family, metrics=c("", "", "", ""), cmap=cmap,
                 cmapEncoding=cmapEncoding, pdfresource=pdfresource)
    class(font) <- "CIDFont"
    checkFont(font)
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
    ## do initialization if needed: not recursive
    initPSandPDFfonts()
    ndots <- length(fonts <- list(...))
    if (ndots == 0L)
        get(".PDF.Fonts", envir=.PSenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0L) {
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
    font$encoding %in% c("default", encoding, paste0(encoding, ".enc"))
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
# Called at first use
#   a) because that's a sensible place to do initialisation of package globals
#   b) because it does not work to do it BEFORE then.  In particular,
#      if the body of this function is evaluated when the R code of the
#      package is sourced, then the method dispatch on checkFont() does
#      not work because when the R code is sourced, the S3 methods in
#      this package have not yet been registered.
#      Also, we want the run-time locale not the install-time locale.

initPSandPDFfonts <- function() {
    if(exists(".PostScript.Options", envir = .PSenv, inherits=FALSE)) return()

assign(".PostScript.Options",
    list(onefile = TRUE,
         family = "Helvetica",
         title = "R Graphics Output",
         fonts = NULL,
	 encoding = "default",
	 bg	= "transparent",
	 fg	= "black",
	 width	= 0,
	 height = 0,
         horizontal = TRUE,
	 pointsize  = 12,
         paper	= "default",
         pagecentre = TRUE,
	 print.it   = FALSE,
	 command    = "default",
         colormodel = "srgb",
         useKerning = TRUE,
         fillOddEven = FALSE), envir = .PSenv)
assign(".PostScript.Options.default",
       get(".PostScript.Options", envir = .PSenv),
       envir = .PSenv)

assign(".PDF.Options",
    list(width	= 7,
	 height = 7,
         onefile = TRUE,
         family = "Helvetica",
         title = "R Graphics Output",
         fonts = NULL,
         version = "1.4",
         paper = "special",
         encoding = "default",
	 bg	= "transparent",
	 fg	= "black",
	 pointsize  = 12,
	 pagecentre = TRUE,
         colormodel = "srgb",
         useDingbats = TRUE,
         useKerning = TRUE,
         fillOddEven = FALSE,
         compress = TRUE), envir = .PSenv)
assign(".PDF.Options.default",
       get(".PDF.Options", envir = .PSenv),
       envir = .PSenv)


postscriptFonts(# Default Serif font is Times
                serif = Type1Font("Times",
                  c("Times-Roman.afm", "Times-Bold.afm",
                    "Times-Italic.afm", "Times-BoldItalic.afm",
                    "Symbol.afm")),
                # Default Sans Serif font is Helvetica
                sans = Type1Font("Helvetica",
                  c("Helvetica.afm", "Helvetica-Bold.afm",
                    "Helvetica-Oblique.afm", "Helvetica-BoldOblique.afm",
                    "Symbol.afm")),
                # Default Monospace font is Courier
                mono = Type1Font("Courier",
                  c("Courier.afm", "Courier-Bold.afm",
                    "Courier-Oblique.afm", "Courier-BoldOblique.afm",
                    "Symbol.afm")),
                # Remainder are standard Adobe fonts that
                # should be present on PostScript devices
                AvantGarde = Type1Font("AvantGarde",
                  c("agw_____.afm", "agd_____.afm",
                    "agwo____.afm", "agdo____.afm",
                    "Symbol.afm")),
                Bookman = Type1Font("Bookman",
                  c("bkl_____.afm", "bkd_____.afm",
                    "bkli____.afm", "bkdi____.afm",
                    "Symbol.afm")),
                Courier = Type1Font("Courier",
                  c("Courier.afm", "Courier-Bold.afm",
                    "Courier-Oblique.afm", "Courier-BoldOblique.afm",
                    "Symbol.afm")),
                Helvetica = Type1Font("Helvetica",
                  c("Helvetica.afm", "Helvetica-Bold.afm",
                    "Helvetica-Oblique.afm", "Helvetica-BoldOblique.afm",
                    "Symbol.afm")),
                "Helvetica-Narrow" = Type1Font("Helvetica-Narrow",
                  c("hvn_____.afm", "hvnb____.afm",
                    "hvno____.afm", "hvnbo___.afm",
                    "Symbol.afm")),
                NewCenturySchoolbook = Type1Font("NewCenturySchoolbook",
                  c("ncr_____.afm", "ncb_____.afm",
                    "nci_____.afm", "ncbi____.afm",
                    "Symbol.afm")),
                Palatino = Type1Font("Palatino",
                  c("por_____.afm", "pob_____.afm",
                    "poi_____.afm", "pobi____.afm",
                    "Symbol.afm")),
                Times = Type1Font("Times",
                  c("Times-Roman.afm", "Times-Bold.afm",
                    "Times-Italic.afm", "Times-BoldItalic.afm",
                    "Symbol.afm")),
                # URW equivalents
                URWGothic = Type1Font("URWGothic",
                  c("a010013l.afm", "a010015l.afm",
                    "a010033l.afm", "a010035l.afm",
                    "s050000l.afm")),
                URWBookman = Type1Font("URWBookman",
                  c("b018012l.afm", "b018015l.afm",
                    "b018032l.afm", "b018035l.afm",
                    "s050000l.afm")),
                NimbusMon = Type1Font("NimbusMon",
                  c("n022003l.afm", "n022004l.afm",
                    "n022023l.afm", "n022024l.afm",
                    "s050000l.afm")),
                NimbusSan = Type1Font("NimbusSan",
                  c("n019003l.afm", "n019004l.afm",
                    "n019023l.afm", "n019024l.afm",
                    "s050000l.afm")),
                URWHelvetica = Type1Font("URWHelvetica",
                  c("n019003l.afm", "n019004l.afm",
                    "n019023l.afm", "n019024l.afm",
                    "s050000l.afm")),
                NimbusSanCond = Type1Font("NimbusSanCond",
                  c("n019043l.afm", "n019044l.afm",
                    "n019063l.afm", "n019064l.afm",
                    "s050000l.afm")),
                CenturySch = Type1Font("CenturySch",
                  c("c059013l.afm", "c059016l.afm",
                    "c059033l.afm", "c059036l.afm",
                    "s050000l.afm")),
                URWPalladio = Type1Font("URWPalladio",
                  c("p052003l.afm", "p052004l.afm",
                    "p052023l.afm", "p052024l.afm",
                    "s050000l.afm")),
                NimbusRom = Type1Font("NimbusRom",
                  c("n021003l.afm", "n021004l.afm",
                    "n021023l.afm", "n021024l.afm",
                    "s050000l.afm")),
                URWTimes = Type1Font("URWTimes",
                  c("n021003l.afm", "n021004l.afm",
                    "n021023l.afm", "n021024l.afm",
                    "s050000l.afm"))
                )

## All of the above Type1 fonts are the same for PostScript and PDF
do.call("pdfFonts", postscriptFonts())

## add ComputerModern to postscript only
postscriptFonts(# Computer Modern as recoded by Brian D'Urso
                ComputerModern = Type1Font("ComputerModern",
                  c("CM_regular_10.afm", "CM_boldx_10.afm",
                    "CM_italic_10.afm", "CM_boldx_italic_10.afm",
                    "CM_symbol_10.afm"), encoding = "TeXtext.enc"),
                 ComputerModernItalic = Type1Font("ComputerModernItalic",
                  c("CM_regular_10.afm", "CM_boldx_10.afm", "cmti10.afm",
                    "cmbxti10.afm", "CM_symbol_10.afm"),
                 encoding = "TeXtext.enc")
                )


# CJK fonts
postscriptFonts(Japan1 = CIDFont("HeiseiKakuGo-W5", "EUC-H", "EUC-JP"),
                Japan1HeiMin = CIDFont("HeiseiMin-W3", "EUC-H", "EUC-JP"),
                Japan1GothicBBB =
                CIDFont("GothicBBB-Medium", "EUC-H", "EUC-JP"),
                Japan1Ryumin = CIDFont("Ryumin-Light", "EUC-H", "EUC-JP"),
                Korea1 = CIDFont("Baekmuk-Batang", "KSCms-UHC-H", "CP949"),
                Korea1deb = CIDFont("Batang-Regular", "KSCms-UHC-H", "CP949"),
                CNS1 = CIDFont("MOESung-Regular", "B5pc-H", "CP950"),
                GB1 = CIDFont("BousungEG-Light-GB", "GBK-EUC-H", "GBK"))

pdfFonts(Japan1 = CIDFont("KozMinPro-Regular-Acro", "EUC-H", "EUC-JP",
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
                 sep = "\n      ")),
         Japan1HeiMin = CIDFont("HeiseiMin-W3-Acro", "EUC-H", "EUC-JP",
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
                 sep = "\n      ")),
         Japan1GothicBBB = CIDFont("GothicBBB-Medium", "EUC-H", "EUC-JP",
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
                 sep = "\n      ")),
         Japan1Ryumin = CIDFont("Ryumin-Light", "EUC-H", "EUC-JP",
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
                 sep = "\n      ")),
         Korea1 = CIDFont("HYSMyeongJoStd-Medium-Acro", "KSCms-UHC-H", "CP949",
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
                 sep = "\n      ")),
         Korea1deb = CIDFont("HYGothic-Medium-Acro", "KSCms-UHC-H", "CP949",
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
                 sep = "\n      ")),
         CNS1 = CIDFont("MSungStd-Light-Acro", "B5pc-H", "CP950",
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
                 sep = "\n      ")),
         GB1 = CIDFont("STSong-Light-Acro", "GBK-EUC-H", "GBK",
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
                 sep = "\n      ")))
}

# Call ghostscript to process postscript or pdf file to embed fonts
# (could also be used to convert ps or pdf to any supported  format)
embedFonts <- function(file, # The ps or pdf file to convert
                       format, # Default guessed from file suffix
                       outfile = file, # By default overwrite file
                       fontpaths = "",
                       options = "" # Additional options to ghostscript
                       )
{
    if(!is.character(file) || length(file) != 1L || !nzchar(file))
        stop("'file' must be a non-empty character string")
    suffix <- gsub(".+[.]", "", file)
    if (missing(format)) {
        format <- switch(suffix,
                         ps = ,
                         eps = "pswrite",
                         pdf = "pdfwrite")
    }
    if (!is.character(format)) {
        stop("Invalid output format")
    }
    gsexe <- Sys.getenv("R_GSCMD")
    if(.Platform$OS.type == "windows" && !nzchar(gsexe))
        gsexe <- Sys.getenv("GSC")
    if(is.null(gsexe) || !nzchar(gsexe)) {
        gsexe <- switch(.Platform$OS.type,
                        unix = "gs",
                        windows = {
                            poss <- Sys.which(c("gswin64c.exe", "gswin32c.exe"))
                            poss <- poss[nzchar(poss)]
                            if(length(poss)) poss else "gswin32c.exe"
                        })
    } else if(.Platform$OS.type == "windows" &&
              length(grep(" ", gsexe, fixed=TRUE)))
        gsexe <- shortPathName(gsexe)
    tmpfile <- tempfile("Rembed")
    if (length(fontpaths))
        fontpaths <-
            shQuote(paste0("-sFONTPATH=",
                           paste(fontpaths, collapse =.Platform$path.sep)))
    cmd <- paste0(gsexe, " -dNOPAUSE -dBATCH -q -dAutoRotatePages=/None -sDEVICE=", format,
                  " -sOutputFile=", tmpfile, " ", fontpaths, " ",
                  options, " ", shQuote(file))
    ret <- system(cmd)
    if(ret != 0)
        stop(gettextf("status %d in running command '%s'", ret, cmd),
             domain = NA)
    file.copy(tmpfile, outfile, overwrite = TRUE)
    invisible(cmd)
}
