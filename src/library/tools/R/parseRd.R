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

parse_Rd <- function(file, srcfile = NULL, encoding = "unknown", verbose = FALSE)
{
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            if (missing(srcfile) && isTRUE(getOption("keep.source")))
        	srcfile <- srcfile(file)
            file <- file(file, "r")
            on.exit(close(file))
        }
    .Internal(parse_Rd(file, srcfile, encoding, verbose))
}

print.Rd <- function(x, ...) {
    pr <- function(x, indent) {
        spaces <- paste(rep(" ", indent), collapse="")
    	if (is.list(x)) {
    	    tag <- attr(x, "Rd_tag")
    	    if (length(grep("^#", tag)) > 0) {
		cat(tag, x[[1]][[1]], "\n")
		x <- x[[2]]
		for (i in seq_along(x)) pr(x[[i]], indent)
		cat("#endif\n")
    	    } else {
		cat(spaces);
		cat(tag);
		if (!is.null(option <- attr(x, "Rd_option"))) {
		    cat("[\n");
		    pr(option, indent + 2)
		    cat(spaces, "]\n", spaces, sep="");
		}
		cat("{\n")
		for (i in seq_along(x)) pr(x[[i]], indent + 2)
		cat(spaces, "}\n", sep="")
    	    }
    	} else cat(paste(strwrap(x, indent=indent, exdent=indent), "\n"),
    	           sep="")
    }
    for (i in seq_along(x)) pr(x[[i]], 0)
}
