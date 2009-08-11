#  File src/library/tools/R/parseRd.R
#  Part of the R package, http://www.R-project.org
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

parse_Rd <- function(file, srcfile = NULL, encoding = "unknown",
                     verbose = FALSE, fragment = FALSE)
{
    if(is.character(file)) {
        file0 <- file
        if(file == "") {
            file <- stdin()
        } else {
            ## keep.source is FALSE in batch use
            ## encoding issues here, for now use file encoding
            if (missing(srcfile)) ## && isTRUE(getOption("keep.source")))
                srcfile <- srcfile(file)
        }
    } else file0 <- "<connection>"
    lines <- readLines(file, warn = FALSE)
    ## remove old-style marking for data, keep line nos
    lines[lines == "\\non_function{}"] <- ""
    ## extract the encoding if marked in the file:
    ## do this in two steps to minimize warnings in MBCS locales
    ## Note this is required to be on a line by itself.
    enc <- grep("\\encoding{", lines, fixed = TRUE, useBytes=TRUE)
    enc <- grep("^\\\\encoding\\{([^}]*)\\}.*", lines[enc], value=TRUE)
    if(length(enc)) {
        if(length(enc) > 1L)
            warning("multiple \\encoding lines in file ", file0)
        ## keep first one
        enc <- enc[1L]
        enc <- sub("^\\\\encoding\\{([^}]*)\\}.*", "\\1", enc)
        if(verbose) message("found encoding ", enc)
        if(enc %in% c("UTF-8", "utf-8", "utf8"))
            encoding <- "UTF-8"
        else
            encoding <- enc
    }
    if (encoding == "unknown") encoding <- ""

    ## the internal function must get some sort of srcfile
    if (!inherits(srcfile, "srcfile"))
    	srcfile <- srcfile(file0)
    basename <- basename(srcfile$filename)
    srcfile$encoding <- encoding

    if (encoding != "UTF-8")
    	lines <- iconv(lines, encoding, "UTF-8", sub = "byte")
    ## In a Latin1 locale, the textConnection will recode everything to
    ## Latin1, so mark it as unknown
    Encoding(lines) <- "unknown"
    tcon <- textConnection(lines)
    on.exit(close(tcon))

    .Internal(parse_Rd(tcon, srcfile, "UTF-8", verbose, basename, fragment))
}

print.Rd <- function(x, ...) {
    cat(as.character.Rd(x), sep="", collapse="")
    invisible(x)
}

as.character.Rd <- function(x, ...) {
    TWOARG <- c("\\section", "\\item", "\\enc", "\\method", "\\S3method",
                "\\S4method", "\\tabular", "\\deqn", "\\eqn")
    pr <- function(x) {
        tag <- attr(x, "Rd_tag")
        if (is.null(tag) || tag == "LIST") tag <- ""
    	if (is.list(x)) {
    	    if (tag == "Rd") { # a whole file
    	        result <- character(0)
    	    	for (i in seq_along(x)) result <- c(result, pr(x[[i]]))
    	    } else if (length(grep("^#", tag))) {
    	    	result <- c(tag, x[[1L]][[1L]])
    	    	x <- x[[2L]]
    	    	for (i in seq_along(x)) result <- c(result, pr(x[[i]]))
    	    	result <- c(result, "#endif\n")
    	    } else if (tag %in% TWOARG) {
    	    	result <- tag
    	    	for (i in seq_along(x)) result <- c(result, pr(x[[i]]))
    	    } else {
    	    	result <- tag
    	    	if (!is.null(option <- attr(x, "Rd_option")))
    	    	    result <- c(result, "[", pr(option), "]")
    	    	result <- c(result, "{")
    	    	for (i in seq_along(x)) result <- c(result, pr(x[[i]]))
    	    	result <- c(result, "}")
    	    }
    	} else result <- as.character(x)
    	result
    }
    if (is.null(attr(x, "Rd_tag")))
    	attr(x, "Rd_tag") <- "Rd"
    pr(x)
}
