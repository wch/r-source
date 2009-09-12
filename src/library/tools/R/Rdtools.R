RdTextFilter <-
function(ifile, encoding = "unknown", keepSpacing = TRUE,
         drop = character(), keep = character())
{
    if (inherits(ifile, "Rd")) p <- ifile
    else p <- parse_Rd(ifile, encoding = encoding)
    tags <- RdTags(p)
    if ("\\encoding" %in% tags) {
	encoding <- p[[which(tags=="\\encoding")]][[1L]]
	if(encoding %in% c("UTF-8", "utf-8", "utf8")) encoding <- "UTF-8"
	p <- parse_Rd(ifile, encoding=encoding)
    } else
	encoding <- ""

    ## Directly using a text connection to accumulate the filtered
    ## output seems to be faster than using .eval_with_capture(): to use
    ## the latter, change mycat to cat, or use mycat <- cat, and create
    ## out via
    ## out <- .eval_with_capture({
    ##     show(p)
    ##     mycat("\n")
    ## })$output
    
    myval <- character()
    mycon <- textConnection("myval", open = "w", local = TRUE,
                            encoding = "UTF-8") 
    on.exit(close(mycon))
    mycat <- function(...) cat(..., file = mycon)

    prevline <- 1L
    prevcol <- 0L

    doPartialMarkup <- function(x, tags, i) { # handle things like \bold{pre}fix
        result <- FALSE
    	if (i < length(tags)
            && tags[i+1L] == "TEXT"
            && length(x[[i]]) == 1L
            && tags[i] %in% c("\\bold", "\\emph", "\\strong", "\\link")
            && !(tags[i] %in% drop)
            && RdTags(x[[i]]) == "TEXT") {
    	    text1 <- x[[i]][[1L]]
    	    if (length(grep("[^[:space:]]$", text1))) { # Ends in non-blank
    	    	text2 <- x[[i+1L]]
    	    	if (length(grep("^[^[:space:]]", text2))) { # Starts non-blank
    	    	    show(text1)
    	    	    prevcol <<- prevcol+1L # Shift text2 left by one column
    	    	    saveline <- prevline
    	    	    show(text2)
    	    	    if (prevline == saveline)
    	    	    	prevcol <<- prevcol-1L
    		    result <- TRUE
    		}
    	    }
	}
	result
    }

    show <- function(x) {
	srcref <- attr(x, "srcref")
	firstline <- srcref[1L]
	firstbyte <- srcref[2L]
	lastline <- srcref[3L]
	lastbyte <- srcref[4L]
	firstcol <- srcref[5L]
	lastcol <- srcref[6L]
	tag <- attr(x, "Rd_tag")
	if (is.null(tag)) tag <- "NULL"
	if (tag %in% drop) tag <- "DROP"
	else if (tag %in% keep) tag <- "KEEPLIST"  # Include both text and lists
	switch(tag,
	KEEP =,
	TEXT = {
	    if (prevline < firstline) {
		prevcol <<- 0L
		mycat(rep.int("\n",
                              if(keepSpacing) firstline - prevline else 1L))
	    }
	    if (keepSpacing)
                mycat(rep.int(" ", firstcol - prevcol - 1L), sep = "")
	    x <- as.character(srcref) # go back to original form
	    mycat(x, sep = "")
	    prevcol <<- lastcol
	    prevline <<- lastline
	},
	"\\docType"=,
	"\\encoding"=,
	"\\keyword"=,
	"\\email"=,
	"\\file"=,
	"\\linkS4class"=,
	"\\pkg"=,
	"\\var"=,
	"\\method"=,
	"\\S3method"=,
	"\\S4method"=,
	"\\link"=,
	DROP = {},  # do nothing

	"\\tabular"=,
	"#ifdef"=,
	"#ifndef"={  # Ignore the first arg, process the second
	    show(x[[2L]])
	},
	"\\item"={   # Ignore the first arg of a two-arg item
	    if (length(x) == 2L) show(x[[2L]])
	},
	{	# default
	    if (is.list(x)) {
             	tags <- RdTags(x)
             	i <- 0L
             	while (i < length(x)) {
             	    i <- i + 1L
             	    if (doPartialMarkup(x, tags, i))
             	    	i <- i + 1L
             	    else
             		show(x[[i]])
             	}
	    } else if (tag == "KEEPLIST") {
	    	attr(x, "Rd_tag") <- "KEEP"
	    	show(x)
	    }
	})
    }

    show(p)
    mycat("\n")
    
    out <- textConnectionValue(mycon)

    ## Ideally, we would always canonicalize to UTF-8.
    ## However, when using RdTextFilter() for aspell(), it is not clear
    ## whether this is a good idea: the aspell program does not need to
    ## have full UTF-8 support (and what precisely holds is not clear:
    ## the manuals says that aspell
    ##   can easily check documents in UTF-8 without having to use a
    ##   special dictionary.
    ## but also
    ##   If Aspell is compiled with a version of the curses library that
    ##   support wide characters then Aspell can also check UTF-8 text. 
    ## So at least until this can be resolved, turn filter results for
    ## Rd files originally in latin1 back to latin1.
    if(encoding == "latin1")
        out <- iconv(out, "UTF-8", "latin1")

    out
}
