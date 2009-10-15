#  File src/library/utils/R/unix/help.R
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

offline_help_helper <- function(texfile, type = "postscript")
{
    PDF <- type == "pdf"
    tools::texi2dvi(texfile, pdf=PDF, clean=TRUE)
    ofile <- sub("tex$", if(PDF) "pdf" else "ps", texfile)
    if(!PDF) {
        dfile <- sub("tex$", "dvi", texfile)
        on.exit(unlink(dfile))
        dvips <- getOption("dvipscmd", default = "dvips")
        res <- system(paste(dvips, dfile, "> /dev/null 2>&1"))
        if(res)
            stop(gettextf("running '%s' failed", dvips), domain = NA)
        if(!file.exists(ofile)) {
            message(gettextf("'%s' produced no output file: sent to printer?",
                             dvips), domain = NA)
            return(invisible())
        }
    } else if(!file.exists(ofile))
        stop(gettextf("creation of '%s' failed", ofile), domain = NA)
    if(ofile != basename(ofile)) file.copy(ofile, basename(ofile))
    message("Saving help page to ", sQuote(basename(ofile)))
    invisible()
}
