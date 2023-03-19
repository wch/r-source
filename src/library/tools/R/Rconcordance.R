##  File src/library/tools/R/Rconcordance.R
##  Part of the R package, https://www.R-project.org
##
## Copyright (C) 1995-2016 The R Core Team
## Copyright (C) 2022 Duncan Murdoch
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  https://www.R-project.org/Licenses/


# There are three kinds of objects used when working with
# concordances.  
#
# 1.  The "activeConcordance" is an environment used to build a 
#     concordance one string at a time.  
# 2.  The "Rconcordance" is a list object.
# 3.  "String concordances" are representations of concordance
#     objects suitable for embedding in text files.

# This function produces an activeConcordance.

activeConcordance <- function(srcfile = NA_character_) 
    local({
	lastSrcref <- NULL
	srcLinenum <- integer()
	srcFile <- srcfile
	offset <- 0
	lastText <- ""
		
	saveSrcref <- function(node) {
	    # Node may be a function which we don't
	    # want to evaluate unnecessarily
	    lastSrcref <<- utils::getSrcref(node)
	}
		
	addToConcordance <- function(text) {
	    if (any(nchar(text) > 0)) {
	    	lastText <<- text
	        concordanceUsed <- length(srcLinenum)	
	        newlines <- sum(nchar(gsub("[^\n]", "", text)))
	        srcLine <- utils::getSrcLocation(lastSrcref, "line")
	        if (!is.null(srcLine)) {
	    	
	    	    # Do we have a later node on the same output
	    	    # line? Save it instead
	            if (concordanceUsed)
	            	if (text[1] != "\n")
	                    concordanceUsed <- concordanceUsed - 1
	                else
	                    newlines <- newlines - 1
	            # Save the current line(s)
	    	    srcLinenum[concordanceUsed + seq_len(1 + newlines)] <<- srcLine
	        } else if (!concordanceUsed)
	            offset <<- offset + newlines
	    }
	}
		
	finish <- function() {
            ## Drop the last line if it is empty
            lastText <<- lastText[[length(lastText)]]
	    if (length(srcLinenum) && (len <- nchar(lastText)) && substr(lastText, len, len) == "\n")
		srcLinenum <- srcLinenum[-length(srcLinenum)]
	    
	    structure(list(offset = offset, srcLine = srcLinenum,
	    	           srcFile = srcFile), 
	    	      class = "Rconcordance")
	}
		
	structure(environment(), class = "activeConcordance")
    })

print.activeConcordance <- function(x, ...) {
    cat("lastSrcref:")
    print(x$lastSrcref)
    cat("lastText:")
    print(x$lastText)
    print(x$finish())
    invisible(x)
}

print.Rconcordance <- function(x, ...) {
    df <- data.frame(srcFile = x$srcFile, srcLine = x$srcLine)
    rownames(df) <- seq_len(nrow(df)) + x$offset
    print(df)
    invisible(x)
}


# This function takes a location in a file and uses a concordance
# object to find the corresponding location in the source for that
# file.

matchConcordance <- function(linenum, concordance) {
    if (!all(c("offset", "srcLine", "srcFile") %in% names(concordance)))
	stop("concordance is not valid")
    linenum <- as.numeric(linenum)
    srcLines <- concordance$srcLine
    srcFile <- rep_len(concordance$srcFile, length(srcLines))
    offset <- concordance$offset
    
    result <- matrix(character(), length(linenum), 2, 
    		     dimnames = list(NULL, 
    		     		    c("srcFile", "srcLine")))
    for (i in seq_along(linenum)) {
	if (linenum[i] <= concordance$offset)
	    result[i,] <- c("", "")
	else
	    result[i,] <- c(srcFile[linenum[i] - offset], 
	    		    with(concordance, srcLine[linenum[i] - offset]))
    }
    result
}

# This function converts concordance objects to string representations
# of them.  

## The string has three or four parts, separated by colons:
## 1.  The output .tex filename
## 2.  The input .Rnw filename
## 3.  Optionally, the starting line number of the output coded as "ofs nn",
##     where nn is the offset to the first output line.  This is omitted if nn is 0.
## 4.  The input line numbers corresponding to each output line.
##     This are compressed using the following simple scheme:
##     The first line number, followed by
##     a run-length encoded diff of the rest of the line numbers.

as.character.Rconcordance <- function(x,
			      targetfile = "",
			      ...) {
    concordance <- x
    offset <- concordance$offset
    src <- concordance$srcLine
    
    result <- character()
    
    srcfile <- rep_len(concordance$srcFile, length(src))
    
    while (length(src)) {
        first <- src[1]
        if (length(unique(srcfile)) > 1)
            n <- which(srcfile != srcfile[1])[1] - 1
        else
            n <- length(srcfile)
        
        vals <- with(rle(diff(src[seq_len(n)])), as.numeric(rbind(lengths, values)))
        result <- c(result, paste0("concordance:", 
               targetfile, ":",
               srcfile[1], ":",
               if (offset) paste0("ofs ", offset, ":"),
               concordance$srcLine[1], " ",
               paste(vals, collapse = " ")
               ))
        offset <- offset + n
        drop <- seq_len(n)
        src <- src[-drop]
        srcfile <- srcfile[-drop]
    }
    result    
}

as.Rconcordance <- function(x, ...) {
    UseMethod("as.Rconcordance")  
}

# This takes concordance strings and combines them
# into one concordance object.

as.Rconcordance.default <- function(x, ...) {
    # clean comments etc.
    s <- sub("^.*(concordance){1}?", "concordance", sub("[^[:digit:]]*$", "", x))
    s <- grep("^concordance:", s, value = TRUE)
    if (!length(s))
    	return(NULL)
    result <- stringToConcordance(s[1])
    for (line in s[-1])
    	result <- addConcordance(result, line)
    result
}

# Windows paths may include colons in the filenames
# if drive letters are used.  This looks for drive letters that
# have been split from the rest of the path and reattaches
# them.

fixWindowsConcordancePaths <- function(split) {
    if (length(split) <= 4)
        return(split)
    # We are looking for a drive letter which should have been at the start
    # of the 2nd or 3rd entry, but will be in an entry by itself
    
    driveletter <- grep("^[a-zA-Z]$", split[2:4]) + 1
    ofs <- grep("^ofs [[:digit:]]+$", split[4:length(split)]) + 3
    
    # The drive letter can't precede the offset record
    driveletter <- setdiff(driveletter, ofs - 1)
    
    if (!length(driveletter))
        return(split)
    
    if (!length(ofs) # no ofs record but length is 5 or more
        || length(split) >= 6) {
        if (2 %in% driveletter) {
            split <- c(split[1],
                       paste(split[2], split[3], sep=":"),
                       split[4:length(split)])
            driveletter <- driveletter - 1
        }
        if (3 %in% driveletter) {
            split <- c(split[1:2],
                       paste(split[3], split[4], sep=":"),
                       split[5:length(split)])
        }
    }
    split
}

# This takes one concordance string and produces a single concordance
# object

stringToConcordance <- function(s) {
    split <- strsplit(s, ":")[[1]]
    if (.Platform$OS.type == "windows")
        split <- fixWindowsConcordancePaths(split)
    targetfile <- split[2]
    srcFile <- split[3]
    if (length(split) == 4) {
    	ofs <- 0
    	vi <- 4
    } else {
    	ofs <- as.integer(sub("^ofs ([0-9]+)", "\\1", split[4]))
    	vi <- 5
    }
    values <- as.integer(strsplit(split[vi], " ")[[1]])
    firstline <- values[1]
    rledata <- matrix(values[-1], nrow = 2)
    rle <- structure(list(lengths=rledata[1,], values=rledata[2,]), class="rle")
    diffs <- inverse.rle(rle)
    srcLines <- c(firstline, firstline + cumsum(diffs))
    structure(list(offset = ofs, srcFile = srcFile, srcLine = srcLines),
    	      class = "Rconcordance")
}

# This modifies an existing concordance object to incorporate
# one new concordance string

addConcordance <- function(conc, s) {
    prev <- stringToConcordance(s)
    if (!is.null(prev)) {
    	conc$srcFile <- rep_len(conc$srcFile, length(conc$srcLine))
        i <- seq_along(prev$srcLine)
        conc$srcFile[prev$offset + i] <- prev$srcFile
        conc$srcLine[prev$offset + i] <- prev$srcLine
    }
    conc
}

# This modifies an existing concordance by following links specified
# in a previous one.

followConcordance <- function(concordance, prevConcordance) {
    if (!is.null(prevConcordance)) {
        curLines <- concordance$srcLine
        curFile <- rep_len(concordance$srcFile, length(curLines))
        curOfs <- concordance$offset
        
        prevLines <- prevConcordance$srcLine
        prevFile <- rep_len(prevConcordance$srcFile, length(prevLines))
        prevOfs <- prevConcordance$offset
        
        if (prevOfs) {
            prevLines <- c(rep(NA_integer_, prevOfs), prevLines)
            prevFile <- c(rep(NA_character_, prevOfs), prevFile)
            prevOfs <- 0
        }
        n0 <- max(curLines)
        n1 <- length(prevLines)
        if (n1 < n0) {
            prevLines <- c(prevLines, rep(NA_integer_, n0 - n1))
            prevFile <- c(prevFile, rep(NA_character_, n0 - n1))
        }
        new <- is.na(prevLines[curLines])
        
        concordance$srcFile <- ifelse(new, curFile,
                                      prevFile[curLines])
        concordance$srcLine <- ifelse(new, curLines,
                                      prevLines[curLines])
    }
    concordance
}
