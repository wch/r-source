#  File src/library/tools/R/Sweavetools.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

    syntax <- utils:::SweaveGetSyntax(ifile)

    ## Read in an re-encode as needed.
    ## Alternatively, could use utils:::SweaveReadFile() ...
    lines <- readLines(ifile, warn = FALSE)
    if(encoding != "unknown") {
        if(encoding == "UTF-8")
            Encoding(lines) <- "UTF-8"
        else
            lines <- iconv(lines, encoding, "", sub = "byte")
    }

    TEXT <- 1L
    CODE <- 0L

    dpos <- grep(syntax$doc, lines)
    cpos <- grep(syntax$code, lines)

    recs <- rbind(data.frame(line = dpos,
                             type = rep.int(TEXT, length(dpos))),
                  data.frame(line = cpos,
                             type = rep.int(CODE, length(cpos)))
                  )
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
