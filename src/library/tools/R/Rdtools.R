
RdTextFilter <-
function(ifile, encoding = "unknown",
         drop = character(0), keep = character(0))
{
    if (inherits(ifile, "Rd")) p <- ifile
    else p <- parse_Rd(ifile, encoding=encoding)
    tags <- RdTags(p)
    if ("\\encoding" %in% tags) {
	encoding <- p[[which(tags=="\\encoding")]][[1]]
	if(encoding %in% c("UTF-8", "utf-8", "utf8")) 
	    encoding <- "UTF-8"
	p <- parse_Rd(ifile, encoding=encoding)
    } else 
	encoding <- ""

    prevline <- 1
    prevcol <- 0
    
    doPartialMarkup <- function(x, tags, i) { # handle things like \bold{pre}fix 
        result <- FALSE
    	if (i < length(tags)
            && tags[i+1] == "TEXT"
            && length(x[[i]]) == 1
            && tags[i] %in% c("\\bold", "\\emph", "\\strong", "\\link")
            && RdTags(x[[i]]) == "TEXT") {
    	    text1 <- x[[i]][[1]]
    	    if (length(grep("[^[:space:]]$", text1))) { # Ends in non-blank
    	    	text2 <- x[[i+1]]
    	    	if (length(grep("^[^[:space:]]", text2))) { # Starts non-blank
    	    	    show(text1)
    	    	    prevcol <<- prevcol+1 # Shift text2 left by one column
    	    	    saveline <- prevline
    	    	    show(text2)
    	    	    if (prevline == saveline)
    	    	    	prevcol <<- prevcol-1
    		    result <- TRUE
    		}
    	    }
	}
	result
    }
    	
    show <- function(x) {
	srcref <- attr(x, "srcref")
	firstline <- srcref[1]
	firstbyte <- srcref[2]
	lastline <- srcref[3]
	lastbyte <- srcref[4]
	firstcol <- srcref[5]
	lastcol <- srcref[6]
	tag <- attr(x, "Rd_tag")
	if (is.null(tag)) tag <- "NULL"
	if (tag %in% drop) tag <- "DROP"
	else if (tag %in% keep) tag <- "KEEPLIST"  # Include both text and lists
	switch(tag,
	KEEP =,
	TEXT = {
	    if (prevline < firstline) {
		prevcol <<- 0
		cat(rep("\n", firstline-prevline))
	    }
	    cat(rep(" ", firstcol - prevcol - 1), sep="")
	    x <- as.character(srcref) # go back to original form
	    if (encoding != "") Encoding(x) <- encoding
	    cat(x,sep="")
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
	    show(x[[2]])
	},
	"\\item"={   # Ignore the first arg of a two-arg item
	    if (length(x) == 2) show(x[[2]])
	},
	{	# default
	    if (is.list(x)) {
             	tags <- RdTags(x)
             	i <- 0
             	while (i < length(x)) {
             	    i <- i + 1
             	    if (doPartialMarkup(x, tags, i))
             	    	i <- i + 1
             	    else
             		show(x[[i]])
             	}
	    } else if (tag == "KEEPLIST") { 
	    	attr(x, "Rd_tag") <- "KEEP" 
	    	show(x)
	    }
	})
    }
    capture.output({
    	show(p)
    	cat("\n")
    })
}
