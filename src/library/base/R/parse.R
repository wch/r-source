#  File src/library/base/R/parse.R
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

parse <- function(file = "", n = NULL, text = NULL, prompt = "?",
                  srcfile = NULL, encoding = "unknown")
{
    keep.source <- isTRUE(getOption("keep.source"))
    if(!is.null(text)) {
    	if (length(text) == 0L) return(expression())
	if (missing(srcfile) && keep.source)
	    srcfile <- srcfilecopy("<text>", text)
    }
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            filename <- file
            file <- file(filename, "r")
            if (missing(srcfile) && keep.source) {
            	text <- readLines(file)
            	if (!length(text)) text <- ""
            	close(file)
            	file <- stdin()
        	srcfile <- srcfilecopy(filename, text, file.info(filename)[1,"mtime"],
        	                       isFile = TRUE)
            } else
                on.exit(close(file))
        }
    .Internal(parse(file, n, text, prompt, srcfile, encoding))
}
