#  File src/library/tools/R/parseLatex.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

## this is called during package installation via makeLatex()
parseLatex <- function(text, filename = deparse(substitute(text)),
                     verbose = FALSE, verbatim = c("verbatim", "verbatim*",
                     "Sinput", "Soutput") )
{
    ## the internal function must get some sort of srcfile
    srcfile <- srcfilecopy(filename, text, file.mtime(filename))
    text <- paste(text, collapse="\n")
    .External2("C_parseLatex", text, srcfile, verbose, as.character(verbatim),
               PACKAGE = "tools")
}


# This converts a latex object into a single element character vector
deparseLatex <- function(x, dropBraces = FALSE)
{
    result <- character()
    lastTag <- "TEXT"
    for (i in seq_along(x)) {
        a <- x[[i]]
        tag <- attr(a, "latex_tag")
        if (is.null(tag)) tag <- "NULL"
        switch(tag,
        VERB = ,
        TEXT = ,
        MACRO = ,
        COMMENT = result <- c(result, a),
        BLOCK = result <- c(result, if (dropBraces && lastTag == "TEXT") deparseLatex(a) else c("{", deparseLatex(a), "}")),
        ENVIRONMENT = result <- c(result,
        	"\\begin{", a[[1L]], "}",
        	deparseLatex(a[[2L]]),
        	"\\end{", a[[1L]], "}"),
        MATH = result <- c(result, "$", deparseLatex(a), "$"),
        NULL = stop("Internal error, no tag", domain = NA)
        )
        lastTag <- tag
    }
    paste(result, collapse="")
}

print.LaTeX <- function(x, ...) cat(deparseLatex(x), "\n")

latex_tag <- function(x, tag)
{
    if (!is.null(x)) attr(x, "latex_tag") <- tag
    x
}

# This makes substitutions within a latex object to replace latex characters
# with UTF8 characters where possible.
latexToUtf8 <- function(x)
{
    i <- 0L
    whitespace <- c(' ', '\t', '\n')
    while (i < length(x)) {
    	i <- i + 1L
        a <- x[[i]]
        tag <- attr(a, "latex_tag")
        if (tag == "MACRO") {
            numargs <- latexArgCount[a]
            if (!is.na(numargs)) { # Do we know this macro?
		args <- vector("list", numargs)
		j <- i
		getNext <- TRUE
		k <- 1L
		while (k <= numargs) {
		    if (getNext) {
			j <- j + 1L
			if (j > length(x)) {
			    warning("argument for ", c(a), " not found", domain = NA)
			    nextobj <- latex_tag("", "TEXT")
			    nexttag <- "TEXT"
			    nextchars <- ""
			} else {
			    nextobj <- x[[j]]
			    nexttag <- attr(nextobj, "latex_tag")
			    if (nexttag == "TEXT")
				nextchars <- strsplit(nextobj, "")[[1L]]
			}
			getNext <- FALSE
		    }
		    switch(nexttag,
			TEXT = {
			    args[[k]] <- latex_tag(nextchars[1L], "TEXT")
			    nextchars <- nextchars[-1L]
			    if (!length(nextchars)) getNext <- TRUE
			    if (args[[k]] %in% whitespace) next
			    k <- k+1L
			},
			COMMENT = getNext <- TRUE, # strip comments
			BLOCK =,
			ENVIRONMENT =,
			MATH = {
			    args[[k]] <- latexToUtf8(nextobj)
			    k <- k+1L
			    getNext <- TRUE
			},
			NULL = stop("Internal error:  NULL tag", domain = NA))
		}
		index <- a
		for (i1 in seq_along(args)) {
		    if (is.null(latexTable[[index]])) break
		    nextobj1 <- args[[i1]]
		    nexttag1 <- attr(nextobj1, "latex_tag")
		    index <- c(index, switch(nexttag1,
			    MACRO =,
			    TEXT = nextobj1,
			    BLOCK = deparseLatex(nextobj1, dropBraces=TRUE)))
		}
		subst <- latex_tag(latexTable[[index]], "TEXT")

		if (!is.null(subst) && !is.list(subst)) { # We've made a substitution, which will always
		  	       	       # be a new latex object, possibly containing UTF8
		    x[[i]] <- subst

		    if (numargs) {
		    	if (nexttag == "TEXT" && length(nextchars)) {
		    	    # We've partially used up the next one, so rebuild it
			    nextobj[1L] <- paste(nextchars, collapse="")
			    x[[j]] <- nextobj
			    j <- j-1L
			}
			while (j > i) {
			    # Remove the used up args
			    x[[j]] <- NULL
			    j <- j-1L
			}
	            }
		} else
		    i <- j
	    }
	} else if (tag == "BLOCK")
	    x[[i]] <- latexToUtf8(a)
    }
    x
}

makeLatexTable <- function(utf8table)
{
    all <- list()
    for (i in seq_along(utf8table)) {
    	if (grepl("^[{].*[}]$", c <- utf8table[i]))
    	    all[[as.character(i)]] <- parseLatex(c)[[1L]]
    }
    table <- list()
    for (i in names(all)) {
    	codepoint <- as.numeric(i)
    	macro <- all[[i]][[1L]]
    	args <- all[[i]][-1L]
    	index <- macro
    	getNext <- TRUE
    	repeat {
	    if (getNext) {
	    	if (!length(args)) break
		nextobj <- args[[1L]]
		args <- args[-1L]
		nexttag <- attr(nextobj, "latex_tag")
		if (nexttag == "TEXT")
		    nextchars <- strsplit(nextobj, "")[[1L]]
		else
		    nextchars <- character()
		getNext <- FALSE
	    }
    	    if (nexttag == "TEXT") {
    	    	if (length(nextchars)) {
    	    	    arg <- nextchars[1L]
    	    	    nextchars <- nextchars[-1L]
    	    	} else {
    	    	    getNext <- TRUE
    	    	    next
    	    	}
    	    } else if (nexttag == "BLOCK") {
    	    	if (!length(nextobj)) {
    	    	    arg <- ""   # or character()?
    	    	    getNext <- TRUE
    	    	} else {
    	    	    arg <- nextobj[[1L]]
    	    	    nextobj <- nextobj[-1L]
    	    	    argtag <- attr(arg, "latex_tag")
    	    	    if (argtag != "TEXT")
    	    	    	stop("internal error", domain = NA)
    	    	}
    	    } else if (nexttag == "MACRO") {
    	    	arg <- nextobj[1L]
    	    	getNext <- TRUE
    	    }
    	    index <- c(index, arg)
    	}
    	repeat{  # Need to record \a macros twice
	    oldArgCount <- latexArgCount[macro]
	    argCount <- length(index) - 1L
	    if (is.na(oldArgCount))
		latexArgCount[macro] <<- argCount
	    else if (oldArgCount != length(index) - 1L)
    	    stop("Inconsistent arg count for ", macro, domain = NA)

	    for (i in seq_along(index)) {
		if (is.null(entry <- table[[index[1L:i]]])) {
		    if (i < length(index))
			table[[index[1L:i]]] <- list()
		    else
			table[[index]] <- intToUtf8(codepoint)
		} else if (!is.list(entry))
		    warning("entry for ", codepoint, "=", index[1L:i], " already defined to be", entry, domain = NA)
	    }
	    if (index[1L] != "\\a") break
	    index <- index[-1L]
	    index[1L] <- macro <- sub("^", "\\\\", index[1L])
	}
    }
    table
}
