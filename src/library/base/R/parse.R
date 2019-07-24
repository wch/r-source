#  File src/library/base/R/parse.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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
#  https://www.R-project.org/Licenses/

parse <- function(file = "", n = NULL, text = NULL, prompt = "?",
		  keep.source = getOption("keep.source"),
                  srcfile = NULL, encoding = "unknown")
{
    keep.source <- isTRUE(keep.source)
    if(!is.null(text)) {
    	if (length(text) == 0L) return(expression())
	if (missing(srcfile)) {
	    srcfile <- "<text>"
	    if (keep.source)
	       srcfile <- srcfilecopy(srcfile, text)
	}
	file <- stdin()
    } else {
	if(is.character(file)) {
            if(file == "") {
            	file <- stdin()
            	if (missing(srcfile))
            	    srcfile <- "<stdin>"
            } else {
		filename <- file
		file <- file(filename, "r")
            	if (missing(srcfile))
            	    srcfile <- filename
            	if (keep.source) {
		    text <- readLines(file, warn = FALSE)
		    if (!length(text)) text <- ""
            	    close(file)
            	    file <- stdin()
        	    srcfile <-
                        srcfilecopy(filename, text, file.mtime(filename),
                                    isFile = TRUE)
                } else
		    on.exit(close(file))
	    }
	}
    }
    .Internal(parse(file, n, text, prompt, srcfile, encoding))
}

## Simple special versions of parse(text = s, keep.source=FALSE) :
if(FALSE) {  # R level implementation (not used):
str2expression <- function(text) parse(text=text, keep.source=FALSE)
str2lang       <- function(s) parse(text=s, keep.source=FALSE)[[1L]]
## For str2lang(), actually, we check 'length 1' twice:
str2lang       <- function(s) {
    stopifnot(length(s) == 1L)
    ex <- parse(text=s, keep.source=FALSE)
    stopifnot(length(ex) == 1L)
    ex[[1L]]
}
}# end{ R level implementation }

str2lang       <- function(s)    .Internal(str2lang(s))
str2expression <- function(text) .Internal(str2expression(text))
