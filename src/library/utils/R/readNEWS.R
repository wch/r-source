readNEWS <- function(file = file.path(R.home(), "NEWS"),
                     trace = FALSE, chop = c("first", "1", "par1", "keepAll"))
{
    ## Purpose: read R's NEWS file - or a file similarly organized
    ## ----------------------------------------------------------------------
    ## Arguments: trace: is used in  "inner functions"
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 26 Jun 2006, 15:34

    p0 <- function(...) paste(..., sep='')

    rmIniTABs	   <- function(ch) sub("^\t+", '', ch)
    rmTABs	   <- function(ch) gsub("\t+", '', ch)
    collapseWSpace <- function(ch) gsub("[\t ]+", " ", ch)
    "%nIN%" <- function(x,table) is.na(match(x, table))

    chop1st <- function(cvec) {
	## The first non-empty line
	n <- length(cvec); if(n <= 1) return(cvec)
	## else	 n >= 2
	empty <- grep("^[\t ]*$", cvec)
	cvec[if(any(!empty)) which(!empty)[1] else 1]
    }

    chopPara <- function(cvec) {
	n <- length(cvec); if(n <= 1) return(cvec)
	## else	 n >= 2
	empty <- grep("^[\t ]*$", cvec)
	## the first non-empty ``from the right''
	nm <- 1:n %nIN% (n + 1 - rev(empty))
	if(any(nm))
	    cvec[1:(n - (which(nm)[1] - 1))]
	else ## all are empty; return just one
	    cvec[1]
    }

### FIXME: default for 'chop' should be (something like) "dropEmptyTrail"

    cl <- match.call()
    E.prefix <- "^    o[\t ]+"
    chop <- match.arg(chop)
    chopFun <- switch(chop,
		      "first" = chop1st,
		      "1" = function(x) x[1],
		      "par1" = chopPara,
		      "keepAll" = function(x)x)

    parseEntry <- function(ll)
    {
	## Purpose: parse a single NEWS entry
	## Arguments: ll: lines of text (character vector)
	nl <- length(ll)
	ll[1] <- sub(E.prefix, '', ll[1])

	##cat("	    entry with",nl, " lines of text")
	chv <- collapseWSpace(ll)
	chopFun(chv)
    }

    parseSection <- function(ll, kind)
    {
	## Purpose: parse one section (e.g., "BUG FIXES") of NEWS
	## Arguments: ll: lines of text (character vector)
	nl <- length(ll)
	if(trace) cat("	    section '", kind,"' : ", nl, " lines", sep='')

	## if(trace) cat(head(ll, min(3, nl)), if(nl > 5) ".............", '', sep="\n")
	## if (nl > 3) if(trace) cat(tail(ll, min(2, nl-3)), '', sep="\n")

	iS <- grep(E.prefix, ll)
	if(trace) cat("	 with ", length(iS), "entries\n")
	entries <- as.list(iS)
	## entries have no labels !

	iS <- c(iS, nl+1L)
	for(i in seq_along(entries))
	    entries[[i]] <- parseEntry(ll[iS[i] : (iS[i+1] - 1L)])
	entries
    }

    parseVersion <- function(ll, ver)
    {
	## Purpose: parse NEWS of one "x.y.z" R version
	## Arguments: ll: lines of text (character vector)
	##	     ver: version number of version, e.g., '2.2.1 patched'
	if(trace) cat("	 parseVersion(*, ver =", ver,"),")
	s.pre <- "^[A-Z]+"
	iC <- grep(s.pre, ll)
	if(trace) cat("	 with ", length(iC), "sections\n")
	sections <- as.list(iC)
	names(sections) <- ll[iC]
	iC <- c(iC, length(ll)+1L) # such that	 iC[i] : (iC[i+1]-1)  makes sense
	for(i in seq_along(sections))
	    sections[[i]] <- parseSection(ll[(iC[i]+ 1L) : (iC[i+1] - 1L)],
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
	names(versions) <- sub(s.pre, '', ll[iC])

	iC <- c(iC, length(ll)+1L) # such that	 iC[i] : (iC[i+1]-1)  makes sense
	for(i in seq_along(versions))
	    versions[[i]] <- parseVersion(ll[(iC[i]+ 1L) : (iC[i+1] - 1L)],
					  ver = names(versions)[i])
	versions
    }

    tfile <- file
    if(is.character(file)) {
        tfile <- normalizePath(file)
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
    nl <- length(ll)
    if(trace) {
        if(is.character(tfile))
            cat("successfully read ", nl, " lines from ",
                sQuote(tfile), "\n", sep="")
        else
            cat("successfully read ", nl, " lines\n", sep="")
    }

    s.pre <- "^\t*\\*[\t ]+ " ##  REGEXP prefix used to identify series begin
    s.post <- " SERIES NEWS"

    iS <- grep(p0(s.pre, "[1-9]\\.[0-9]", s.post), ll)
    series <- as.list(iS)
    names(series) <- sub(p0(s.post,"[\t ]*\\*$"), '',
			 sub(s.pre, '', ll[iS]))
    if(trace) {
        cat(s.post, ":\n")
        print(unlist(series))
        cat("Now parsing each: ...\n")
    }

    iS <- c(iS, nl+1L) # such that  iS[i] : (iS[i+1]-1)  makes sense
    for(i in seq_along(series))
	series[[i]] <- parseSeries(ll[(iS[i]+ 1L) : (iS[i+1] - 1L)],
				   ver = names(series)[i])
    attr(series, "call") <- cl
    class(series) <- "newsTree"
    series
}
