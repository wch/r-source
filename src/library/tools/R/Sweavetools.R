#  File src/library/tools/R/Sweavetools.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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


SweaveTeXFilter <-
function(ifile, encoding = "unknown")
{
    if(inherits(ifile, "srcfile"))
        ifile <- ifile$filename

    lines <- readLines(ifile, encoding = encoding, warn = FALSE)

    syntax <- utils:::SweaveGetSyntax(ifile)

    TEXT <- 1L
    CODE <- 0L

    recs <- rbind( data.frame(line = grep(syntax$doc, lines),
                              type = TEXT),
                   data.frame(line = grep(syntax$code, lines),
                              type = CODE))
    recs <- recs[order(recs$line),]
    last <- 0L
    state <- TEXT
    for (i in seq_len(nrow(recs))) {
    	line <- recs$line[i]
    	if (state == CODE)
    	    lines[(last+1L):line] <- ""
    	else
    	    lines[line] <- ""
    	state <- recs$type[i]
    	last <- line
    }
    lines
}
