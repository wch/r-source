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

## unused
.show_help_on_topic_as_HTML <-
function(file, topic, warn = TRUE)
{
    ofile <- file
    ## We need to use the version in per-session dir if we can.
    third_base_name <-
        file.path(basename(dirname(dirname(file))),
                  basename(dirname(file)),
                  basename(file))
    ## (Ouch.)
    lnkfile <-
        file.path(tempdir(), ".R", "library", third_base_name)
    if(any(ex <- file.exists(lnkfile))) {
        file <- lnkfile[ex][1L]          # could be more than one
    }
    if(warn && file == ofile) {
        msg <- gettext("Using non-linked HTML file: hyperlinks may be incorrect")
        warning(paste(strwrap(msg), collapse = "\n"))
    }
    file <- paste("file://", URLencode(file), sep = "")
    if(is.null(browser <- getOption("browser")))
        stop("options(\"browser\") not set")
    browseURL(file)
    if (is.character(browser)) {
        writeLines(c(paste("Help for", sQuote(topic),
                           "is shown in browser", browser, "..."),
                     "Use",
                     paste("\thelp(\"", topic, "\", htmlhelp = FALSE)",
                           sep = ""),
                     "or\n\toptions(htmlhelp = FALSE)\nto revert."))
    }
    return(invisible())
}


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
