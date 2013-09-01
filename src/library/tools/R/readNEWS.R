#  File src/library/tools/R/readNEWS.R
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

readNEWS <- function(file = file.path(R.home(), "NEWS"),
                     trace = FALSE, chop = c("first", "1", "par1", "keepAll"))
{
    .Deprecated()
    ## Purpose: read R's NEWS file - or a file similarly organized
    ## ----------------------------------------------------------------------
    ## Arguments: trace: is used in  "inner functions"
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 26 Jun 2006, 15:34

    rmIniTABs	   <- function(ch) sub("^\t+", "", ch)
    rmTABs	   <- function(ch) gsub("\t+", "", ch)
    collapseWSpace <- function(ch) {
        ## This used to be
        ##   gsub("[\t ]+", " ", ch)
        ## which removes sentence-end double spaces.
        ch <- gsub("[\t ]{2,}", "  ", ch)
        gsub("\t", " ", ch)
    }
    "%nIN%" <- function(x,table) is.na(match(x, table))

    chop1st <- function(cvec) {
	## The first non-empty line
	n <- length(cvec); if(n <= 1L) return(cvec)
	## else	 n >= 2
	empty <- grep("^[\t ]*$", cvec)
	cvec[if(any(!empty)) which.max(!empty) else 1L]
    }

    chopPara <- function(cvec) {
	n <- length(cvec); if(n <= 1L) return(cvec)
	## else	 n >= 2
	empty <- grep("^[\t ]*$", cvec)
	## the first non-empty ``from the right''
	nm <- 1L:n %nIN% (n + 1 - rev(empty))
	if(any(nm))
	    cvec[1L:(n - (which.max(nm) - 1L))]
	else ## all are empty; return just one
	    cvec[1L]
    }

### FIXME: default for 'chop' should be (something like) "dropEmptyTrail"

    cl <- match.call()
    E.prefix <- "^    o[\t ]+"
    chop <- match.arg(chop)
    chopFun <- switch(chop,
		      "first" = chop1st,
		      "1" = function(x) x[1L],
		      "par1" = chopPara,
		      "keepAll" = function(x)x)

    parseEntry <- function(ll)
    {
	## Purpose: parse a single NEWS entry
	## Arguments: ll: lines of text (character vector)
	nl <- length(ll)
	ll[1L] <- sub(E.prefix, "", ll[1L])

	##cat("	    entry with",nl, " lines of text")
	chv <- collapseWSpace(ll)
	chopFun(chv)
    }

    parseSection <- function(ll, kind)
    {
	## Purpose: parse one section (e.g., "BUG FIXES") of NEWS
	## Arguments: ll: lines of text (character vector)
	nl <- length(ll)
	if(trace) cat("	    section '", kind,"' : ", nl, " lines", sep = "")

	## if(trace) cat(head(ll, min(3, nl)), if(nl > 5) ".............", "", sep="\n")
	## if (nl > 3) if(trace) cat(tail(ll, min(2, nl-3)), "", sep = "\n")

	iS <- grep(E.prefix, ll)
	if(trace) cat("	 with ", length(iS), "entries\n")
	entries <- as.list(iS)
	## entries have no labels !

	iS <- c(iS, nl+1L)
	for(i in seq_along(entries))
	    entries[[i]] <- parseEntry(ll[iS[i] : (iS[i+1L] - 1L)])
	entries
    }

    parseVersion <- function(ll, ver)
    {
	## Purpose: parse NEWS of one "x.y.z" R version
	## Arguments: ll: lines of text (character vector)
	##	     ver: version number of version, e.g., '2.2.1 patched'
	if(trace) cat("	 parseVersion(*, ver =", ver,"),")
	s.pre <- "^[A-Z0-9]+"
	iC <- grep(s.pre, ll)
	if(trace) cat("	 with ", length(iC), "sections\n")
	sections <- as.list(iC)
	names(sections) <- ll[iC]
	iC <- c(iC, length(ll)+1L) # such that	 iC[i] : (iC[i+1L]-1)  makes sense
	for(i in seq_along(sections))
	    sections[[i]] <- parseSection(ll[(iC[i]+ 1L) : (iC[i+1L] - 1L)],
					  kind = names(sections)[i])
	sections
    }

    parseSeries <- function(ll, ver)
    {
	## Purpose: parse NEWS of full (half year) "x.y" series of R version
	## Arguments: ll: lines of text (character vector)
	##	     ver: version number of series, e.g., '2.3'
	if(trace) cat("\nparseSeries(*, ver =", ver,"):\n")
	s.pre <- "^[\t ]*CHANGES IN R VERSION "
	iC <- grep(s.pre, ll)
	versions <- as.list(iC)
	names(versions) <- sub(s.pre, "", ll[iC])

	iC <- c(iC, length(ll)+1L) # such that	 iC[i] : (iC[i+1L]-1)  makes sense
	for(i in seq_along(versions))
	    versions[[i]] <- parseVersion(ll[(iC[i]+ 1L) : (iC[i+1L] - 1L)],
					  ver = names(versions)[i])
	versions
    }

    # Check if the lines are in a native encoding
    # but have a UTF-8 byte-order mark

    hasBOM <- function(lines) {
        length(lines) >= 1L &&
    	Encoding(line <- lines[1L]) == "unknown" &&
    	nchar(line, type="bytes") >= 3 &&
    	identical( as.integer(charToRaw(line)[1:3]),
    	                         c(0xefL, 0xbbL, 0xbfL) )
    }

    tfile <- file
    if(is.character(file)) {
        tfile <- normalizePath(file) # used for trace message.
        file <- file(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    if(!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    ## We could read in parts ...
    ll <- readLines(file)
    if (hasBOM(ll)) Encoding(ll) <- "UTF-8"

    nl <- length(ll)
    if(trace) {
        if(is.character(tfile))
            cat("successfully read ", nl, " lines from ",
                sQuote(tfile), "\n", sep = "")
        else
            cat("successfully read ", nl, " lines\n", sep = "")
    }

    s.pre <- "^\t*\\*[\t ]+ " ##  REGEXP prefix used to identify series begin
    s.post <- " SERIES NEWS"

    iS <- grep(paste0(s.pre, "[1-9]+\\.[0-9]+", s.post), ll)
    series <- as.list(iS)
    names(series) <- sub(paste0(s.post,"[\t ]*\\*$"), "",
			 sub(s.pre, "", ll[iS]))
    if(trace) {
        cat(s.post, ":\n")
        print(unlist(series))
        cat("Now parsing each: ...\n")
    }

    iS <- c(iS, nl+1L) # such that  iS[i] : (iS[i+1L]-1)  makes sense
    ## At least for 'keepAll', we need to get rid of the whole series
    ## header (could also do this in general, of course):
    if(chop == "keepAll") {
        hl <- grep(sprintf("^\t%s",
                           paste(rep.int("\\*", 30), collapse = "")),
                   ll)
        for(i in seq_along(series))
            series[[i]] <-
                parseSeries(ll[(hl[2L * i] + 1L) :
                               (hl[2L * i + 1L] - 1L)],
                            ver = names(series)[i])
    } else {
        for(i in seq_along(series))
            series[[i]] <-
                parseSeries(ll[(iS[i] + 1L) : (iS[i+1L] - 1L)],
                            ver = names(series)[i])
    }
    attr(series, "call") <- cl
    class(series) <- "newsTree"
    series
}

# Check for common formatting errors in a NEWS file.

checkNEWS <- function(file = file.path(R.home(), "NEWS")) {
    .Deprecated()
    check <- function(item) {
	if (is.list(item)) return(all(unlist(lapply(item, check))))

	if (length(grep("^ o[[:blank:]]", item))) {
	    cat("Item marker found within item:\n", paste(item, collapse="\n"), "\n\n")
	    return(FALSE)
	}
	return(TRUE)
    }

    check(readNEWS(file, chop="keepAll") )
}
