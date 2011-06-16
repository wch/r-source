#  File src/library/base/R/parse.R
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

parse <- function(file = "", n = NULL, text = NULL, prompt = "?",
                  srcfile = NULL, encoding = "unknown")
{
    keep.source <- isTRUE(getOption("keep.source"))
    if(!is.null(text)) {
    	if (length(text) == 0L)
	    return(expression())
	if (missing(srcfile) && keep.source)
	    srcfile <- srcfilecopy("<text>", text)
    }
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            if (missing(srcfile) && keep.source)
        	srcfile <- srcfile(file, Enc = encoding)
            file <- file(file, "r")
            on.exit(close(file))
        }
    .Internal(parse(file, n, text, prompt, srcfile, encoding))
}
